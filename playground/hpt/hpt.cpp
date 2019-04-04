#include <iostream>
#include <vector>
#include <unordered_map>
#include <unordered_set>

/*
  node_item_t = set<simple_type_t> | set<location>
  node_data_t = vector<node_item_t>

  heap:       loc -> node_value_t
  registers:  reg -> value_t

  node_value_t  = unordered_map<tag_t, node_data_t>
  value_t       = set<simple_type_t> | node_value_t | set<location>
*/
using namespace std;

//typedef unordered_set<int32_t> node_item_t ; // 0 or positive = location ; negative = simple type
//typedef vector<node_item_t> node_data_t;

typedef unordered_map<uint32_t, vector<unordered_set<int32_t>>> node_set_t;

// NOTE: GRIN is a typed language, a register can have only one type from the following options: simple_type, location, node
class value_t {
public:
  unordered_set<int32_t>  simple_type;
  node_set_t              node_set;
};

vector<node_set_t>  _memory;
vector<value_t>     _register;

typedef int32_t reg_t;
typedef int32_t mem_t;

bool changed = false;

inline void insert_int_set(unordered_set<int32_t>& dst, int32_t value) {
    unordered_set<int32_t>::iterator got = dst.find (value);

    if ( got == dst.end() ) {
      dst.insert(value);
      changed = true;
    }
}

inline void union_int_set(unordered_set<int32_t>& src, unordered_set<int32_t>& dst) {
  /* TODO: use more efficient operations
      https://lemire.me/blog/2017/01/27/how-expensive-are-the-union-and-intersection-of-two-unordered_set-in-c/
      https://en.cppreference.com/w/cpp/algorithm/set_union
      idea: use vector<int32_t> instead of unordered_set<int32_t>
  */

  for (auto& src_i: src) insert_int_set(dst, src_i);
}

inline void union_node_set(node_set_t& src, node_set_t& dst) {
  for (auto& src_t_v: src) {
    node_set_t::iterator dst_t_v = dst.find (src_t_v.first);

    if ( dst_t_v == dst.end() ) {
      dst.insert(src_t_v);
      changed = true;
    } else if (src_t_v.second.size() != dst_t_v->second.size()) {
        cout << "error: union_node_set" << endl;
    } else {
      for (unsigned i=0; i<src_t_v.second.size(); i++) {
        union_int_set(src_t_v.second.at(i), dst_t_v->second.at(i));
      }
    }
  }
}

inline void union_value(value_t& src, value_t& dst) {
  union_int_set(src.simple_type, dst.simple_type);
  union_node_set(src.node_set, dst.node_set);
}

// data-flow instruction execution
void cmd_move(reg_t src, reg_t dst) {
  union_value(_register[src], _register[dst]);
}

void cmd_store(reg_t src, mem_t dst) {
  union_node_set(_register[src].node_set, _memory[dst]);
}

void cmd_update(reg_t src, reg_t addresses) {
  for (auto& dst: _register[addresses].simple_type)
    if (dst >= 0) union_node_set(_register[src].node_set, _memory[dst]);
}

void cmd_fetch(reg_t addresses, reg_t dst) {
  for (auto& src: _register[addresses].simple_type)
    if (src >= 0) union_node_set(_memory[src], _register[dst].node_set);
}

// set instructions
void cmd_set_simple_type_or_location(reg_t dst, int32_t value) {
  insert_int_set(_register[dst].simple_type, value);
}

void cmd_set_node_type(reg_t dst, int32_t tag, int32_t arity) {
  node_set_t::iterator got = _register[dst].node_set.find (tag);

  if ( got == _register[dst].node_set.end() ) {
    _register[dst].node_set[tag] = vector<unordered_set<int32_t>>(arity);
    changed = true;
  } else if (got->second.size() < arity) {
    got->second.resize(arity);
    changed = true;
  }
}

void cmd_set_node_item(reg_t dst, int32_t tag, int32_t index, int32_t value) {
  node_set_t::iterator got = _register[dst].node_set.find (tag);
  if ( got != _register[dst].node_set.end() && got->second.size() >= index) {
    insert_int_set(got->second[index], value);
  }
}
/*
  operations:
    - register value type check
    - register += register  ; value_t += value_t
    - register += location  ; value_t += node_set_t
    - location += register  ; node_set_t += value_t (only the node set part)
    - register += register (only nodes, i.e. node bind)
    - register += simple type constant
    - register += tag constant
    - register += location constant
    - register += node constant
*/

/*
  case dispatch - valid for the following types
    node (tag value)
    tag value
    simple type value

  bind
    full value - always ok
    node deconstruct - ok if
      - have only node values
      - pattern arity not smaller then the biggest node
*/
