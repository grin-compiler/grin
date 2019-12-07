#include <iostream>
#include <fstream>
#include <iterator>
#include <vector>

#include "IR.h"

/*
  binary format:
  everything is int32

struct range_t {
struct predicate_t {
struct condition_t {
struct selector_t {
struct cmd_t {

  memory count    i32
  register count  i32
  cmd count       i32
  cmds ...
  block count     i32
  blocks (ranges) ...
  intset count    i32
    set size      i32
    set elems ... [i32]
*/


struct ctx_t {
  int32_t *ptr;
  bool error;
};

void read_error(ctx_t &ctx, int code) {
  ctx.error = true;
  std::cout << "error code " << code << "\n";
}

void read_range(ctx_t &ctx, range_t &range) {
  if (ctx.error) return;
  //std::cout << "read_range" << "\n";

  range.from = *ctx.ptr++;
  range.to = *ctx.ptr++;
}

void read_predicate(ctx_t &ctx, predicate_t &predicate) {
  if (ctx.error) return;

  predicate.type = (predicate_type) (*ctx.ptr++);
  //std::cout << "read_predicate " << predicate.type << "\n";
  switch (predicate.type) {
    case PRE_TAG_IN:
    case PRE_TAG_NOT_IN:
      predicate.tag_set_id = *ctx.ptr++;
      break;
    case PRE_VALUE_IN:
    case PRE_VALUE_NOT_IN:
      read_range(ctx, predicate.range);
      break;
    default:
      read_error(ctx,1);
  }
}

void read_condition(ctx_t &ctx, condition_t &condition) {
  if (ctx.error) return;

  condition.type = (condition_type) (*ctx.ptr++);
  //std::cout << "read_condition " << condition.type << "\n";
  switch (condition.type) {
    case CON_NODE_TYPE_EXISTS:
      condition.tag = *ctx.ptr++;
      break;
    case CON_SIMPLE_TYPE_EXISTS:
      condition.simple_type = *ctx.ptr++;
      break;
    case CON_ANY_NOT_IN:
      condition.tag_set_id = *ctx.ptr++;
      break;
    case CON_ANY:
      read_predicate(ctx, condition.predicate);
      break;
    default:
      read_error(ctx,2);
  }
}

void read_selector(ctx_t &ctx, selector_t &selector) {
  if (ctx.error) return;

  selector.type = (selector_type) (*ctx.ptr++);
  //std::cout << "read_selector " << selector.type << "\n";

  switch (selector.type) {
    case SEL_NODE_ITEM:
      selector.node_tag = *ctx.ptr++;
      selector.item_index = *ctx.ptr++;
      break;
    case SEL_CONDITION_AS_SELECTOR:
      read_condition(ctx, selector.condition);
      break;
    case SEL_ALL_FIELDS:
      break;
    default:
      read_error(ctx,3);
  }
}

std::set<int32_t> const_bucket;

void read_constant(ctx_t &ctx, constant_t &constant) {
  if (ctx.error) return;

  constant.type = (constant_type) (*ctx.ptr++);
  //std::cout << "read_constant " << constant.type << "\n";

  switch (constant.type) {
    case CONST_SIMPLE_TYPE:
      constant.simple_type = *ctx.ptr++;
      const_bucket.insert(constant.simple_type);
      break;
    case CONST_HEAP_LOCATION:
      constant.mem = *ctx.ptr++;
      const_bucket.insert(constant.mem);
      break;
    case CONST_NODE_TYPE:
      constant.node_tag = *ctx.ptr++;
      constant.item_index = *ctx.ptr++;
      const_bucket.insert(constant.node_tag);
      break;
    case CONST_NODE_ITEM:
      constant.node_tag = *ctx.ptr++;
      constant.item_index = *ctx.ptr++;
      constant.item_value = *ctx.ptr++;
      const_bucket.insert(constant.node_tag);
      const_bucket.insert(constant.item_value);
      break;
    default:
      read_error(ctx,4);
  }
}

void read_cmd(ctx_t &ctx, cmd_t &c) {
  if (ctx.error) return;

  c.type = (cmd_type) (*ctx.ptr++);

  //std::cout << "read_cmd " << c.type << "\n";

  switch (c.type) {
    case CMD_IF:
      read_condition(ctx, c.cmd_if.condition);
      c.cmd_if.src_reg = *ctx.ptr++;
      c.cmd_if.block_id = *ctx.ptr++;
      break;
    case CMD_PROJECT:
      read_selector(ctx, c.cmd_project.src_selector);
      c.cmd_project.src_reg = *ctx.ptr++;
      c.cmd_project.dst_reg = *ctx.ptr++;
      break;
    case CMD_EXTEND:
      c.cmd_extend.src_reg = *ctx.ptr++;
      read_selector(ctx, c.cmd_extend.dst_selector);
      c.cmd_extend.dst_reg = *ctx.ptr++;
      break;
    case CMD_MOVE:
      c.cmd_move.src_reg = *ctx.ptr++;
      c.cmd_move.dst_reg = *ctx.ptr++;
      break;
    case CMD_RESTRICTED_MOVE:
      c.cmd_restricted_move.src_reg = *ctx.ptr++;
      c.cmd_restricted_move.dst_reg = *ctx.ptr++;
      break;
    case CMD_CONDITIONAL_MOVE:
      c.cmd_conditional_move.src_reg = *ctx.ptr++;
      read_predicate(ctx, c.cmd_conditional_move.predicate);
      c.cmd_conditional_move.dst_reg = *ctx.ptr++;
      break;
    case CMD_FETCH:
      c.cmd_fetch.address_reg = *ctx.ptr++;
      c.cmd_fetch.dst_reg = *ctx.ptr++;
      break;
    case CMD_STORE:
      c.cmd_store.src_reg = *ctx.ptr++;
      c.cmd_store.address = *ctx.ptr++;
      break;
    case CMD_UPDATE:
      c.cmd_update.src_reg = *ctx.ptr++;
      c.cmd_update.address_reg = *ctx.ptr++;
      break;
    case CMD_RESTRICTED_UPDATE:
      c.cmd_restricted_update.src_reg = *ctx.ptr++;
      c.cmd_restricted_update.address_reg = *ctx.ptr++;
      break;
    case CMD_CONDITIONAL_UPDATE:
      c.cmd_conditional_update.src_reg = *ctx.ptr++;
      read_predicate(ctx, c.cmd_conditional_update.predicate);
      c.cmd_conditional_update.address_reg = *ctx.ptr++;
      break;
    case CMD_SET:
      c.cmd_set.dst_reg = *ctx.ptr++;
      read_constant(ctx, c.cmd_set.constant);
      break;
    default:
      read_error(ctx,5);
  }
}

void read_abstract_program(ctx_t &ctx, abstract_program_t &prg) {
  int32_t count, i, size, j;

  // memory and register count
  prg.memory_count = *ctx.ptr++;
  prg.register_count = *ctx.ptr++;

  // start block id
  prg.start_block_id = *ctx.ptr++;

  // debug
  std::cout << "prg.memory_count " << prg.memory_count << "\n";
  std::cout << "prg.register_count " << prg.register_count << "\n";
  std::cout << "prg.start_block_id " << prg.start_block_id << "\n";

  // commands
  count = *ctx.ptr++;
  std::cout << "command count " << count << "\n";
  prg.cmd.resize(count);
  for (i = 0; i < count; i++) {
    read_cmd(ctx, prg.cmd[i]);
    if (ctx.error) return;
  }

  // blocks
  count = *ctx.ptr++;
  std::cout << "block count " << count << "\n";
  prg.block.resize(count);
  for (i = 0; i < count; i++) {
    read_range(ctx, prg.block[i]);
  }

  // int sets
  count = *ctx.ptr++;
  std::cout << "intset count " << count << "\n";
  prg.intset.resize(count);
  for (i = 0; i < count; i++) {
    size = *ctx.ptr++;
    std::cout << "intset size " << size << "\n";
    for (j = 0; j < size; j++) {
      prg.intset[i].insert(*ctx.ptr++);
    }
  }
}

abstract_program_t *load_abstract_program(char *name) {

  // load file
  std::ifstream input(name, std::ios::binary);
  std::vector<unsigned char> buffer(std::istreambuf_iterator<char>(input), {});

  // setup context
  ctx_t ctx;
  ctx.error = false;
  ctx.ptr = (int32_t*) buffer.data();

  // read program
  abstract_program_t *prg = new abstract_program_t();

  read_abstract_program(ctx, *prg);

  std::cout << "const_bucket size " << const_bucket.size() << "\n";
  int32_t const_min = INT16_MAX;
  int32_t const_max = INT16_MIN;
  for (auto& c : const_bucket) {
    const_min = std::min(const_min,c);
    const_max = std::max(const_max,c);
  }
  std::cout << "const_min " << const_min << "\n";
  std::cout << "const_max " << const_max << "\n";
  std::cout << "sizeof(int32_t) " << sizeof(int32_t) << "\n";

  // return result
  if (ctx.error) {
    delete prg;
    return 0;
  }

  return prg;
}
