#pragma once

#include <cstring>
#include <iostream>
#include <cstddef>
#include <stdexcept>
#include <vector>
#include <bitset>
#include <cassert>
#include <functional>

template<typename T>
class BitTrie
{
  struct Node{
    bool valid{false};
    T data{};
    Node* children[2];
    Node* at(bool i){return children[i];}
  };
  Node* root{new Node()};

public:
  BitTrie() = default;
  
  template<size_t size>
  void add(std::bitset<size> key, T value)
  {
    // std::cout << key << std::endl;
    Node* n = root;
    for(int i=size-1;i>=0;i--)
    {
      const bool bit = key[i];
      // std::cout << bit;
      if(!n->at(bit))
        n->children[bit] = new Node();
      n = n->at(bit);
    }
    // std::cout <<"\n\n"<< std::endl ;
    if(n->valid) 
      throw std::invalid_argument("[BINTRIE] {"+key.to_string()+"} already present");
    n->valid = true;
    n->data = value;
  }

  template<size_t size>
  void add(std::vector<std::pair<std::bitset<size>, T>> elements)
  {
    for(auto element : elements)
      add(element.first, element.second);
  }

  void add(std::vector<std::bitset<8>> keys, T element)
  {
    
  }

  template<size_t size>
  T* find(std::bitset<size> bits) const
  {
    Node* n = root;
    for(int i=size-1;i>=0;i--)
    {
      const bool bit = bits[i];
      if(n->at(bit) == nullptr) break;
      n = n->at(bit);
    }
    if(!n->valid) return nullptr;
    return &n->data;
  }
};

// '1' - evaluates to static 1
// '*' - evaluates to any bit
// default - evaluates to static 0
inline std::vector<std::bitset<8>> operator ""_c(const char* pattern, std::size_t size){
  assert(size == 8 && "pattern must have length 8");
  std::vector<std::bitset<8>> res{};
  std::bitset<8> perm;
  for(int i=0;i<8;i++) perm[7-i] = (pattern[i]=='1');

  std::function<void(int)> permutate 
  = [&](int i=0){
    while(i<8 && pattern[7-i] != '*')
      i++;

    if(i==8){
        res.push_back(perm);
        return;
    }
    perm.reset(i);
    permutate(i+1);
    perm.set(i);
    permutate(i+1);
  };
  permutate(0);
  return res;
} 

