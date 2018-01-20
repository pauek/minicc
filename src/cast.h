/*
 * Copyright 2017 WebAssembly Community Group participants
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef CAST_H
#define CAST_H

#include <memory>
#include <type_traits>
#include <assert.h>

#define STATIC_ASSERT(x) static_assert((x), #x)

template <typename Derived, typename Base>
bool isa(const Base* obj) {
   STATIC_ASSERT((std::is_base_of<Base, Derived>::value));
   return Derived::is_instance(obj);
}

template <typename Derived, typename Base>
const Derived* cast(const Base* obj) {
   assert(isa<Derived>(obj));
   return static_cast<const Derived*>(obj);
};

template <typename Derived, typename Base>
Derived* cast(Base* obj) {
   assert(isa<Derived>(obj));
   return static_cast<Derived*>(obj);
};

#endif
