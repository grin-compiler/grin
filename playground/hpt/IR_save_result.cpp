#include <iostream>
#include <fstream>
#include <iterator>
#include <vector>
#include <algorithm>

#include "IR.h"

/*
  save mem
  save reg
  mem node sets
  reg values
*/

void emit_int32_t(std::ofstream& f, int32_t data) {
  f.write((char*)&data,sizeof(data));
}

void emit_int_set(std::ofstream& f, int_set_t& s) {
  emit_int32_t(f, RES_INT_SET);
  emit_int32_t(f, s.size());
  for (auto& i: s) {
    emit_int32_t(f, i);
  }
}

void emit_node_item(std::ofstream& f, std::vector<int_set_t>& ni) {
  emit_int32_t(f, RES_NODE_ITEM);
  emit_int32_t(f, ni.size());
  for (auto& i: ni) {
    emit_int_set(f, i);
  }
}

void emit_node_set(std::ofstream& f, node_set_t& ns) {
  emit_int32_t(f, RES_NODE_SET);
  emit_int32_t(f, ns.size());

  for (auto& i: ns) {
    emit_int32_t(f, i.first);
    emit_node_item(f, i.second);
  }
}

void emit_value(std::ofstream& f, value_t& v) {
  emit_int32_t(f, RES_VALUE);
  emit_int_set(f, v.simple_type);
  emit_node_set(f, v.node_set);
}

void save_result(std::ofstream& f, int32_t iter_count, std::vector<node_set_t>& mem, std::vector<value_t>& reg) {
  emit_int32_t(f, iter_count);
  emit_int32_t(f, mem.size());
  emit_int32_t(f, reg.size());
  for (auto& i: mem) {
    emit_node_set(f, i);
  }
  for (auto& i: reg) {
    emit_value(f, i);
  }
}

void save_result_file(const char* name, int32_t iter_count, std::vector<node_set_t>& mem, std::vector<value_t>& reg) {
  //std::cout << "save result to: " << name << "\n";

  std::ofstream fout(name, std::ios::out | std::ios::binary);

  save_result(fout, iter_count, mem, reg);

  fout.close();
}
