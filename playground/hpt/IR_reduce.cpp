#include <iostream>
#include <vector>
#include <unordered_map>
#include <unordered_set>

#include "IR.h"

typedef std::unordered_map<int32_t, std::vector<std::unordered_set<int32_t>>> node_set_t;

// NOTE: GRIN is a typed language, a register can have only one type from the following options: simple_type, location, node
struct value_t {
  std::unordered_set<int32_t> simple_type;
  node_set_t                  node_set;
};

struct computer_state_t {
  std::vector<node_set_t> mem;
  std::vector<value_t>    reg;
  bool                    changed;
  bool                    error;
  abstract_program_t&     prg;

  computer_state_t(abstract_program_t& p) : prg(p) {
    mem.resize(p.memory_count);
    reg.resize(p.register_count);
  }

  // predicate functions
  bool eval_predicate_int(int32_t i, predicate_t& p);
  bool eval_predicate_tag(int32_t i, predicate_t& p);

  // modification functions
  void insert_int_set(std::unordered_set<int32_t>& dst, int32_t value);

  void conditional_union_int_set(std::unordered_set<int32_t>& src, std::unordered_set<int32_t>& dst, predicate_t& p);
  void conditional_union_node_set(node_set_t& src, node_set_t& dst, predicate_t& p);
  void conditional_union_value(value_t& src, value_t& dst, predicate_t& p);

  void union_int_set(std::unordered_set<int32_t>& src, std::unordered_set<int32_t>& dst);
  void union_node_set(node_set_t& src, node_set_t& dst, bool merge_new_tags = true);
  void union_value(value_t& src, value_t& dst);

  // eval
  void eval_cmd(cmd_t &c);
  void eval_block(block_id_t bid);

  // eval commands
  void eval_conditional_update(cmd_t &c);
  void eval_conditional_move(cmd_t &c);
  void eval_restricted_move(cmd_t &c);
  void eval_restricted_update(cmd_t &c);
  void eval_move(cmd_t &c);
  void eval_fetch(cmd_t &c);
  void eval_store(cmd_t &c);
  void eval_update(cmd_t &c);
  void eval_set(cmd_t &c);
};

// value modification

inline void computer_state_t::insert_int_set(std::unordered_set<int32_t>& dst, int32_t value) {
    std::unordered_set<int32_t>::iterator got = dst.find (value);

    if ( got == dst.end() ) {
      dst.insert(value);
      changed = true;
    }
}

inline void computer_state_t::union_int_set(std::unordered_set<int32_t>& src, std::unordered_set<int32_t>& dst) {
  /* TODO: use more efficient operations
      https://lemire.me/blog/2017/01/27/how-expensive-are-the-union-and-intersection-of-two-unordered_set-in-c/
      https://en.cppreference.com/w/cpp/algorithm/set_union
      idea: use vector<int32_t> instead of unordered_set<int32_t>
  */

  for (auto& src_i: src) insert_int_set(dst, src_i);
}

inline void computer_state_t::union_node_set(node_set_t& src, node_set_t& dst, bool merge_new_tags) {
  for (auto& src_t_v: src) {
    node_set_t::iterator dst_t_v = dst.find (src_t_v.first);

    if ( dst_t_v == dst.end() ) {
      if (merge_new_tags) {
        dst.insert(src_t_v);
        changed = true;
      }
    } else if (src_t_v.second.size() != dst_t_v->second.size()) {
        std::cout << "error: union_node_set\n";
        error = true;
    } else {
      for (unsigned i=0; i<src_t_v.second.size(); i++) {
        union_int_set(src_t_v.second.at(i), dst_t_v->second.at(i));
      }
    }
  }
}

inline void computer_state_t::union_value(value_t& src, value_t& dst) {
  union_int_set(src.simple_type, dst.simple_type);
  union_node_set(src.node_set, dst.node_set);
}

inline bool computer_state_t::eval_predicate_int(int32_t i, predicate_t& p) {
  switch (p.type) {
    case PRE_VALUE_IN:
      return p.range.from <= i && i < p.range.to;
      break;
    case PRE_VALUE_NOT_IN:
      return p.range.from > i || i >= p.range.to;
      break;
    default:
      break;
  }
  return true;
}

inline bool computer_state_t::eval_predicate_tag(int32_t t, predicate_t& p) {
  switch (p.type) {
    case PRE_TAG_IN:
      return prg.intset[p.tag_set_id].find(t) != prg.intset[p.tag_set_id].end();
      break;
    case PRE_TAG_NOT_IN:
      return prg.intset[p.tag_set_id].find(t) == prg.intset[p.tag_set_id].end();
      break;
    default:
      break;
  }
  return true;
}

inline void computer_state_t::conditional_union_int_set(std::unordered_set<int32_t>& src, std::unordered_set<int32_t>& dst, predicate_t& p) {
  for (auto& src_i: src) {
    if (eval_predicate_int(src_i, p)) {
      insert_int_set(dst, src_i);
    }
  }
}

inline void computer_state_t::conditional_union_node_set(node_set_t& src, node_set_t& dst, predicate_t& p) {
  for (auto& src_t_v: src) {

    if (!eval_predicate_tag(src_t_v.first, p)) continue;

    node_set_t::iterator dst_t_v = dst.find (src_t_v.first);

    if ( dst_t_v == dst.end() ) {
      // case A) new tag in dst node set

      // create new vector of int sets
      node_set_t::mapped_type dst_vector(src_t_v.second.size());

      // fill int set vector accoding the predicate
      for (unsigned i=0; i<src_t_v.second.size(); i++) {
        conditional_union_int_set(src_t_v.second.at(i), dst_vector[i], p);
      }
      // add to dst
      dst.insert({src_t_v.first, dst_vector});
      changed = true;

    } else if (src_t_v.second.size() != dst_t_v->second.size()) {
        std::cout << "error: conditonal_union_node_set\n";
        error = true;

    } else {
      // case B) existing tag in dst node set

      for (unsigned i=0; i<src_t_v.second.size(); i++) {
        conditional_union_int_set(src_t_v.second.at(i), dst_t_v->second.at(i), p);
      }
    }
  }
}

inline void computer_state_t::conditional_union_value(value_t& src, value_t& dst, predicate_t& p) {
  conditional_union_int_set(src.simple_type, dst.simple_type, p);
  conditional_union_node_set(src.node_set, dst.node_set, p);
}

// command evaluation

inline void computer_state_t::eval_conditional_move(cmd_t &c) {
  conditional_union_value(reg[c.cmd_conditional_move.src_reg], reg[c.cmd_conditional_move.dst_reg], c.cmd_conditional_move.predicate);
}

inline void computer_state_t::eval_conditional_update(cmd_t &c) {
  for (auto& dst: reg[c.cmd_conditional_update.address_reg].simple_type) {
    if (dst >= 0) {
      conditional_union_node_set(reg[c.cmd_conditional_update.src_reg].node_set, mem[dst], c.cmd_conditional_update.predicate);
    }
  }
}

inline void computer_state_t::eval_restricted_move(cmd_t &c) {
  /*
     NOTE: same as Move, but only considers tags already present in dstReg
           (basically a Move but only for the common tags)
  */
  value_t& src = reg[c.cmd_restricted_move.src_reg];
  value_t& dst = reg[c.cmd_restricted_move.dst_reg];

  union_int_set(src.simple_type, dst.simple_type);
  union_node_set(src.node_set, dst.node_set, false);
}

inline void computer_state_t::eval_restricted_update(cmd_t &c) {
  for (auto& dst: reg[c.cmd_restricted_update.address_reg].simple_type) {
    if (dst >= 0) {
      union_node_set(reg[c.cmd_restricted_update.src_reg].node_set, mem[dst], false);
    }
  }
}


inline void computer_state_t::eval_move(cmd_t &c) {
  union_value(reg[c.cmd_move.src_reg], reg[c.cmd_move.dst_reg]);
}

inline void computer_state_t::eval_fetch(cmd_t &c) {
  for (auto& src: reg[c.cmd_fetch.address_reg].simple_type) {
    if (src >= 0) {
      union_node_set(mem[src], reg[c.cmd_fetch.dst_reg].node_set);
    }
  }
}

inline void computer_state_t::eval_store(cmd_t &c) {
  union_node_set(reg[c.cmd_store.src_reg].node_set, mem[c.cmd_store.address]);
}

inline void computer_state_t::eval_update(cmd_t &c) {
  for (auto& dst: reg[c.cmd_update.address_reg].simple_type) {
    if (dst >= 0) {
      union_node_set(reg[c.cmd_update.src_reg].node_set, mem[dst]);
    }
  }
}

inline void computer_state_t::eval_set(cmd_t &c) {
  reg_t dst = c.cmd_set.dst_reg;

  switch (c.cmd_set.constant.type) {
    case CONST_SIMPLE_TYPE:
      insert_int_set(reg[dst].simple_type, c.cmd_set.constant.simple_type);
      break;

    case CONST_HEAP_LOCATION:
      insert_int_set(reg[dst].simple_type, c.cmd_set.constant.mem);
      break;

    case CONST_NODE_TYPE: {
        tag_t   tag   = c.cmd_set.constant.node_tag;
        int32_t arity = c.cmd_set.constant.item_index;

        node_set_t::iterator got = reg[dst].node_set.find (tag);

        if ( got == reg[dst].node_set.end() ) {
          reg[dst].node_set[tag] = std::vector<std::unordered_set<int32_t>>(arity);
          changed = true;
        } else if (got->second.size() < arity) {
          got->second.resize(arity);
          changed = true;
        }
      }
      break;

    case CONST_NODE_ITEM: {
        tag_t   tag   = c.cmd_set.constant.node_tag;
        int32_t index = c.cmd_set.constant.item_index;
        int32_t value = c.cmd_set.constant.item_value;

        node_set_t::iterator got = reg[dst].node_set.find (tag);
        if ( got != reg[dst].node_set.end() && got->second.size() >= index) {
          insert_int_set(got->second[index], value);
        }
      }
      break;

    default:
      error = true;
      break;
  }
}

inline void computer_state_t::eval_cmd(cmd_t &c) {
  switch (c.type) {
    case CMD_IF:
      // TODO
      //  condition
      //  predicate
      break;

    case CMD_PROJECT:
      // TODO
      //  selector
      //  condition
      //  predicate
      break;

    case CMD_EXTEND:
      // TODO
      //  selector
      //  condition
      //  predicate
      break;

    case CMD_MOVE:
      eval_move(c);
      break;

    case CMD_RESTRICTED_MOVE:
      eval_restricted_move(c);
      break;

    case CMD_CONDITIONAL_MOVE:
      eval_conditional_move(c);
      break;

    case CMD_FETCH:
      eval_fetch(c);
      break;

    case CMD_STORE:
      eval_store(c);
      break;

    case CMD_UPDATE:
      eval_update(c);
      break;

    case CMD_RESTRICTED_UPDATE:
      eval_restricted_update(c);
      break;

    case CMD_CONDITIONAL_UPDATE:
      eval_conditional_update(c);
      break;

    case CMD_SET:
      eval_set(c);
      break;

    default:
      error = true;
      break;
  }
}

inline void computer_state_t::eval_block(block_id_t bid) {
  for (int pc = prg.block[bid].from; pc < prg.block[bid].to; pc++) {
    eval_cmd(prg.cmd[pc]);
    if (error) return;
  }
}
