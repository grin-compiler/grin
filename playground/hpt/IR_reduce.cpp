#include <iostream>
#include <fstream>

#include "IR.h"

struct computer_state_t {
  std::vector<node_set_t> mem;
  std::vector<value_t>    reg;
  bool                    changed;
  bool                    error;
  abstract_program_t&     prg;
  int                     executed_commands;
  int                     change_count;
  std::ofstream outfile;

  computer_state_t(abstract_program_t& p) : prg(p) {
    mem.resize(p.memory_count);
    reg.resize(p.register_count);
    changed = false;
    error = false;
    executed_commands = 0;
    change_count = 0;
    outfile.open("/home/csaba/df_debug/insts_cpp.dat", std::ios_base::app);
    //outfile.open("/dev/null", std::ios_base::app);
  }

  // predicate and condition functions
  bool eval_predicate_int_set(int_set_t& s, predicate_t& p);
  bool eval_predicate_int(int32_t i, predicate_t& p);
  bool eval_predicate_tag(int32_t i, predicate_t& p);
  bool eval_condition(value_t& v, condition_t& c);

  // modification functions
  void insert_int_set(int_set_t& dst, int32_t value);

  void conditional_union_int_set(int_set_t& src, int_set_t& dst, predicate_t& p);
  void conditional_union_node_set(node_set_t& src, node_set_t& dst, predicate_t& p);
  void conditional_union_value(value_t& src, value_t& dst, predicate_t& p);

  void union_int_set(int_set_t& src, int_set_t& dst);
  void union_node_set_item(tag_t src_tag, std::vector<int_set_t>& src_node_items, node_set_t& dst, bool merge_new_tags = true);
  void union_node_set(node_set_t& src, node_set_t& dst, bool merge_new_tags = true);
  void union_value(value_t& src, value_t& dst);

  // eval
  void eval_cmd(cmd_t &c);
  void eval_block(block_id_t bid);

  // eval commands
  void eval_if(cmd_t &c);
  void eval_project(cmd_t &c);
  void eval_extend(cmd_t &c);
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

inline void computer_state_t::insert_int_set(int_set_t& dst, int32_t value) {
    int_set_t::iterator got = dst.find (value);

    if ( got == dst.end() ) {
      dst.insert(value);
      changed = true;
      change_count++;
    }
}

inline void computer_state_t::union_int_set(int_set_t& src, int_set_t& dst) {
  /* TODO: use more efficient operations
      https://lemire.me/blog/2017/01/27/how-expensive-are-the-union-and-intersection-of-two-unordered_set-in-c/
      https://en.cppreference.com/w/cpp/algorithm/set_union
      idea: use vector<int32_t> instead of unordered_set<int32_t>
  */

  for (auto& src_i: src) insert_int_set(dst, src_i);
}

inline void computer_state_t::union_node_set_item(tag_t src_tag, std::vector<int_set_t>& src_node_items, node_set_t& dst, bool merge_new_tags) {
  node_set_t::iterator dst_t_v = dst.find(src_tag);

  if ( dst_t_v == dst.end() ) {
    if (merge_new_tags) {
      dst.insert({src_tag, src_node_items});
      changed = true;
      change_count++;
    }
  } else if (src_node_items.size() != dst_t_v->second.size()) {
      std::cout << "error: union_node_set_item\n";
      outfile << "error: union_node_set_item\n";
      error = true;
  } else {
    for (unsigned i=0; i<src_node_items.size(); i++) {
      union_int_set(src_node_items.at(i), dst_t_v->second.at(i));
    }
  }
}

inline void computer_state_t::union_node_set(node_set_t& src, node_set_t& dst, bool merge_new_tags) {
  for (auto& src_t_v: src) {
    union_node_set_item(src_t_v.first, src_t_v.second, dst, merge_new_tags);
  }
}

inline void computer_state_t::union_value(value_t& src, value_t& dst) {
  union_int_set(src.simple_type, dst.simple_type);
  union_node_set(src.node_set, dst.node_set);
}

inline bool computer_state_t::eval_predicate_int_set(int_set_t& s, predicate_t& p) {
  for (auto& i: s) {
    if (eval_predicate_int(i, p)) {
      return true;
    }
  };
  return false;
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

inline void computer_state_t::conditional_union_int_set(int_set_t& src, int_set_t& dst, predicate_t& p) {
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
      change_count++;

    } else if (src_t_v.second.size() != dst_t_v->second.size()) {
        std::cout << "error: conditonal_union_node_set\n";
        outfile << "error: conditonal_union_node_set\n";
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

inline bool computer_state_t::eval_condition(value_t& v, condition_t& c) {
  switch (c.type) {
    case CON_NODE_TYPE_EXISTS:
      return v.node_set.count(c.tag) > 0;
      break;
    case CON_SIMPLE_TYPE_EXISTS:
      return v.simple_type.count(c.simple_type) > 0;
      break;
    case CON_ANY_NOT_IN:
      if (v.simple_type.size() > 0) {
        return true;
      }
      for (auto& t_v: v.node_set) {
        if (prg.intset[c.tag_set_id].find(t_v.first) == prg.intset[c.tag_set_id].end()) {
          return true;
        }
      }
      break;
    case CON_ANY:
      switch (c.predicate.type) {
        case PRE_TAG_IN:
        case PRE_TAG_NOT_IN:
          for (auto& t_v: v.node_set) {
            if (eval_predicate_tag(t_v.first, c.predicate)) {
              return true;
            }
          }
          break;
        case PRE_VALUE_IN:
        case PRE_VALUE_NOT_IN:
          if (eval_predicate_int_set(v.simple_type, c.predicate)) {
            return true;
          }
          // iterate node sets
          for (auto& t_v: v.node_set) {
            // iterate node items
            for (auto& s: t_v.second) {
              if (eval_predicate_int_set(s, c.predicate)) {
                return true;
              }
            }
          }
          break;
        default:
          error = true;
          break;
      }
      break;
    default:
      error = true;
      break;
  }
  return false;
}

// command evaluation

inline void computer_state_t::eval_project(cmd_t &c) {
  value_t& src = reg[c.cmd_project.src_reg];
  value_t& dst = reg[c.cmd_project.dst_reg];

  switch (c.cmd_project.src_selector.type) {
    case SEL_NODE_ITEM: {
        outfile << "SEL_NODE_ITEM ";
        int32_t idx = c.cmd_project.src_selector.item_index;
        tag_t   tag = c.cmd_project.src_selector.node_tag;

        node_set_t::iterator src_t_v = src.node_set.find(tag);

        if ( src_t_v == src.node_set.end() ) {
          // ignore if the tag does not exist
          return;
        }
        if (idx >= src_t_v->second.size()) {
          std::cout << "error: project - item index is out of range\n";
          outfile << "error: project - item index is out of range\n";
          outfile << " idx: " << idx;
          outfile << " node size: " << src_t_v->second.size();
          outfile << " tag: " << tag;
          outfile << " src_reg: " << c.cmd_project.src_reg;
          outfile << "\n";
          error = true;
        } else {
          union_int_set(src_t_v->second.at(idx), dst.simple_type);
        }
      }
      break;
    case SEL_CONDITION_AS_SELECTOR: {
        outfile << "SEL_CONDITION_AS_SELECTOR ";
        condition_t& cond = c.cmd_project.src_selector.condition;
        switch (cond.type) {
          case CON_NODE_TYPE_EXISTS: {
              outfile << "CON_NODE_TYPE_EXISTS ";
              node_set_t::iterator src_t_v = src.node_set.find(cond.tag);

              if ( src_t_v == src.node_set.end() ) {
                // ignore if the tag does not exist
                return;
              }
              union_node_set_item(cond.tag, src_t_v->second, dst.node_set);
            }
            break;

          case CON_SIMPLE_TYPE_EXISTS:
            outfile << "CON_SIMPLE_TYPE_EXISTS ";
            if (src.simple_type.count(cond.simple_type) > 0) {
              insert_int_set(dst.simple_type, cond.simple_type);
            }
            break;

          case CON_ANY_NOT_IN:
            outfile << "CON_ANY_NOT_IN ";
            if (eval_condition(src, cond)) {
              union_int_set(src.simple_type, dst.simple_type);
              for (auto& t_v: src.node_set) {
                if (prg.intset[cond.tag_set_id].find(t_v.first) == prg.intset[cond.tag_set_id].end()) {
                  // union if not in the given tags
                  union_node_set_item(t_v.first, t_v.second, dst.node_set);
                }
              }
            }
            break;

          default:
            error = true;
            break;
        }
      }
      break;
    case SEL_ALL_FIELDS:
      outfile << "SEL_ALL_FIELDS ";
      // iterate node sets
      for (auto& t_v: src.node_set) {
        // iterate node items
        for (auto& s: t_v.second) {
          union_int_set(s, dst.simple_type);
        }
      }
      break;
    default:
      error = true;
      break;
  }
}


inline void computer_state_t::eval_extend(cmd_t &c) {
  value_t& src = reg[c.cmd_extend.src_reg];
  value_t& dst = reg[c.cmd_extend.dst_reg];

  switch (c.cmd_extend.dst_selector.type) {
    case SEL_NODE_ITEM: {
        outfile << "SEL_NODE_ITEM ";
        int32_t idx = c.cmd_extend.dst_selector.item_index;
        tag_t   tag = c.cmd_extend.dst_selector.node_tag;

        node_set_t::iterator dst_t_v = dst.node_set.find(tag);

        if ( dst_t_v == dst.node_set.end() ) {
          // ignore if the tag does not exist
          //error = true;
          return;
        }
        if (idx >= dst_t_v->second.size()) {
          std::cout << "error: extend - item index is out of range\n";
          outfile << "error: extend - item index is out of range\n";
          outfile << " idx: " << idx;
          outfile << " node size: " << dst_t_v->second.size();
          outfile << " tag: " << tag;
          outfile << " dst_reg: " << c.cmd_extend.dst_reg;
          outfile << "\n";
          error = true;
        } else {
          union_int_set(src.simple_type, dst_t_v->second.at(idx));
        }
      }
      break;
    case SEL_ALL_FIELDS:
      outfile << "SEL_ALL_FIELDS ";
      for (auto& t_v: dst.node_set) {
        for (auto& s: t_v.second) {
          union_int_set(src.simple_type, s);
        }
      }
      break;
    default:
      error = true;
      break;
  }
}

inline void computer_state_t::eval_if(cmd_t &c) {
  if (eval_condition(reg[c.cmd_if.src_reg], c.cmd_if.condition)) {
    eval_block(c.cmd_if.block_id);
  }
}

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
  value_t& dst = reg[c.cmd_set.dst_reg];

  switch (c.cmd_set.constant.type) {
    case CONST_SIMPLE_TYPE:
      outfile << "CONST_SIMPLE_TYPE ";
      insert_int_set(dst.simple_type, c.cmd_set.constant.simple_type);
      break;

    case CONST_HEAP_LOCATION:
      outfile << "CONST_HEAP_LOCATION ";
      insert_int_set(dst.simple_type, c.cmd_set.constant.mem);
      break;

    case CONST_NODE_TYPE: {
        outfile << "CONST_NODE_TYPE ";
        tag_t   tag   = c.cmd_set.constant.node_tag;
        int32_t arity = c.cmd_set.constant.item_index;

        node_set_t::iterator dst_t_v = dst.node_set.find(tag);

        if ( dst_t_v == dst.node_set.end() ) {
          dst.node_set.insert({tag, std::vector<int_set_t>(arity)});
          changed = true;
          change_count++;
        } else if (dst_t_v->second.size() != arity) {
          error = true;
        }
      }
      break;

    case CONST_NODE_ITEM: {
        outfile << "CONST_NODE_ITEM ";
        tag_t   tag   = c.cmd_set.constant.node_tag;
        int32_t index = c.cmd_set.constant.item_index;
        int32_t value = c.cmd_set.constant.item_value;

        node_set_t::iterator dst_t_v = dst.node_set.find(tag);
        if ( dst_t_v != dst.node_set.end() && index < dst_t_v->second.size()) {
          insert_int_set(dst_t_v->second.at(index), value);
        } else {
          //error = true;
        }
      }
      break;

    default:
      error = true;
      break;
  }
}

inline void computer_state_t::eval_cmd(cmd_t &c) {
  // debug
  char name[2048];
  sprintf(name, "/home/csaba/df_debug/prg_cpp_inst_%04d.dat", executed_commands);
  save_result_file(name, 0, mem, reg);

  if (executed_commands % 1000 == 0) {
    printf(" * changes: %d\t commands: %d \n", change_count, executed_commands);
  }

  executed_commands++;

  outfile << executed_commands << " ";
  outfile.flush();

  switch (c.type) {
    case CMD_IF:
      outfile << "CMD_IF " << std::flush;
      eval_if(c);
      break;

    case CMD_PROJECT:
      outfile << "CMD_PROJECT " << std::flush;
      eval_project(c);
      break;

    case CMD_EXTEND:
      outfile << "CMD_EXTEND " << std::flush;
      eval_extend(c);
      break;

    case CMD_MOVE:
      outfile << "CMD_MOVE " << std::flush;
      eval_move(c);
      break;

    case CMD_RESTRICTED_MOVE:
      outfile << "CMD_RESTRICTED_MOVE " << std::flush;
      eval_restricted_move(c);
      break;

    case CMD_CONDITIONAL_MOVE:
      outfile << "CMD_CONDITIONAL_MOVE " << std::flush;
      eval_conditional_move(c);
      break;

    case CMD_FETCH:
      outfile << "CMD_FETCH " << std::flush;
      eval_fetch(c);
      break;

    case CMD_STORE:
      outfile << "CMD_STORE " << std::flush;
      eval_store(c);
      break;

    case CMD_UPDATE:
      outfile << "CMD_UPDATE " << std::flush;
      eval_update(c);
      break;

    case CMD_RESTRICTED_UPDATE:
      outfile << "CMD_RESTRICTED_UPDATE " << std::flush;
      eval_restricted_update(c);
      break;

    case CMD_CONDITIONAL_UPDATE:
      outfile << "CMD_CONDITIONAL_UPDATE " << std::flush;
      eval_conditional_update(c);
      break;

    case CMD_SET:
      outfile << "CMD_SET " << std::flush;
      eval_set(c);
      break;

    default:
      error = true;
      break;
  }
  outfile << "\n";
}

inline void computer_state_t::eval_block(block_id_t bid) {
  outfile << "\n";
  for (int pc = prg.block[bid].from; pc < prg.block[bid].to; pc++) {
    eval_cmd(prg.cmd[pc]);
    if (error) return;
  }
}

void eval_abstract_program(char *name) {
  abstract_program_t *prg = load_abstract_program(name);
  if (!prg) {
    printf("load error\n");
    return;
  }
  computer_state_t s(*prg);

  int cnt = 0;
  do {
    cnt++;
    s.changed = false;
    s.eval_block(prg->start_block_id);
    printf("iter: %d\tchanges: %d\t commands: %d \n", cnt, s.change_count, s.executed_commands);

    // debug
    // save result after each iteration
    char iname[2048];
    sprintf(iname, "/home/csaba/df_debug/prg_cpp_iter_%04d.dat", cnt);
    save_result_file(iname, 0, s.mem, s.reg);

  } while (s.changed && !s.error);


  if (s.error) {
    printf("dataflow ERROR!\n");
    s.outfile << "dataflow ERROR!\n";
  } else {
    printf("OK\n");
    s.outfile << "OK\n";
  }

  printf("iterations: %d\n", cnt);
  printf("executed_commands: %d\n", s.executed_commands);
  printf("change_count: %d\n", s.change_count);

  // save the result
  std::string res_name(name);
  res_name += ".dat";
  save_result_file(res_name.c_str(), cnt, s.mem, s.reg);

  delete prg;
}
