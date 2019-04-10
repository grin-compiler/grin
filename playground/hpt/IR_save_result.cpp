#include <iostream>
#include <fstream>
#include <iterator>
#include <vector>

#include "IR.h"

/*
  save mem
  save reg
  mem node sets
  reg values
*/

void emit_int_set(std::vector<int32_t>& b, std::unordered_set<int32_t>& s) {
  b.push_back(RES_INT_SET);
  b.push_back(s.size());
  for (auto& i: s) {
    b.push_back(i);
  }
}

void emit_node_item(std::vector<int32_t>& b, std::vector<std::unordered_set<int32_t>>& ni) {
  b.push_back(RES_NODE_ITEM);
  b.push_back(ni.size());
  for (auto& i: ni) {
    emit_int_set(b, i);
  }
}

void emit_node_set(std::vector<int32_t>& b, node_set_t& ns) {
  b.push_back(RES_NODE_SET);
  b.push_back(ns.size());
  for (auto& i: ns) {
    b.push_back(i.first);
    emit_node_item(b, i.second);
  }
}

void emit_value(std::vector<int32_t>& b, value_t& v) {
  b.push_back(RES_VALUE);
  emit_int_set(b, v.simple_type);
  emit_node_set(b, v.node_set);
}

void save_result(std::vector<int32_t>& b, int32_t iter_count, std::vector<node_set_t>& mem, std::vector<value_t>& reg) {
  b.push_back(iter_count);
  b.push_back(mem.size());
  b.push_back(reg.size());
  for (auto& i: mem) {
    emit_node_set(b, i);
  }
  for (auto& i: reg) {
    emit_value(b, i);
  }
}
