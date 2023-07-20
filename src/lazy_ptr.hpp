#ifndef AUTOMATA_LAZY_PTR_HPP
#define AUTOMATA_LAZY_PTR_HPP
//
// lazy_ptr -- reference to an object with copy-on-write semantics
//
//
// constexpr lazy_ptr() noexcept;
//
// Constructs a reference to the NULL object
// -------------------------------------------------
//
// constexpr lazy_ptr(nullptr_t) noexcept;
//
// Same as lazy_ptr()
// -------------------------------------------------
//
// constexpr lazy_ptr(T* p) noexcept;
//
// Constructs a reference to the object pointed to
// by 'p' and takes ownership of it if it's not NULL
//
// -------------------------------------------------
//
// constexpr lazy_ptr& release() noexcept;
//
// Invalidates the reference, maybe destroying the
// underlying object if its owner and there are no
// users, and sets the reference to refer to the
// NULL object
// -------------------------------------------------
//
// constexpr lazy_ptr& use(const lazy_ptr& o);
//
// Releases the reference and constructs a non-ow-
// ning reference to the object that 'o' refers to
// -------------------------------------------------
//
// constexpr lazy_ptr& own(lazy_ptr&& o);
//
// Releases the reference and constructs a reference
// to the object that 'o' refers to, taking owner-
// ship of it if 'o' is its owner, then invalidates
// 'o'
// -------------------------------------------------
//
// constexpr lazy_ptr(const lazy_ptr& o);
//
// Constructs a non-owning reference to the object
// that 'o' refers to
// -------------------------------------------------
// 
// constexpr lazy_ptr(lazy_ptr&& o);
//
// Constructs a reference to the object that 'o'
// refers to, taking ownership of it if 'o' is its
// owner, and invalidates 'o'
// -------------------------------------------------
//
// constexpr lazy_ptr& operator=(const lazy_ptr& o);
//
// Same as use(o)
// -------------------------------------------------
//
// constexpr lazy_ptr& operator=(lazy_ptr&& o);
//
// Same as own(o)
// -------------------------------------------------
//
// constexpr lazy_ptr& operator=(nullptr_t) noexcept;
//
// Same as release()
// -------------------------------------------------
//
// constexpr ~lazy_ptr() noexcept;
//
// Same as release()
// -------------------------------------------------
//
// constexpr bool owns() const noexcept;
//
// Returns true if owns the underlying object
// -------------------------------------------------
//
// constexpr bool lone_owner() const noexcept;
//
// Returns true if owns the underlying object and
// there are no users of that object
// -------------------------------------------------
//
// constexpr bool valid() const noexcept;
//
// Returns true if the reference does not refer to
// the NULL object
// -------------------------------------------------
//
// constexpr operator bool() const noexcept;
//
// Same as valid()
// -------------------------------------------------
//
// constexpr const T* read() const noexcept;
//
// Returns a read-only pointer to the underlying
// object
// -------------------------------------------------
//
// constexpr operator const T*() const noexcept;
//
// Same as read()
// -------------------------------------------------
//
// constexpr const T* operator->() const noexcept;
//
// Read-only access to the members of the underlying
// object. Undefined behaviour if the underlying ob-
// ject is NULL
// -------------------------------------------------
//
// constexpr const T& cref() const noexcept;
//
// Returns a read-only reference to the underlying
// object. Undefined behaviour if the underlying ob-
// ject is NULL
// -------------------------------------------------
//
// constexpr const T& operator*() const noexcept;
//
// Same as cref()
// -------------------------------------------------
//
// constexpr T* write();
//
// If lone_owner(), returns a pointer to the under-
// lying object. Otherwise makes a copy of the ob-
// ject, becomes the owner of the new object, and
// returns a pointer to that new object
// -------------------------------------------------
//
// constexpr T& ref();
//
// If lone_owner(), returns a reference to the
// underlying object. Otherwise makes a copy of the
// object, becomes the owner of the new object, and
// returns a reference to that new object. Undefined
// behaviour if the returned object would have been
// NULL
// -------------------------------------------------
//
// constexpr T* ptr() noexcept;
//
// Returns a raw pointer to the underlying object.
// Any modifications to the object accessed through
// the pointer will be seen by all of its users
// -------------------------------------------------
//
// constexpr const T* ptr() const noexcept;
//
// Returns a raw pointer to the underlying object
// -------------------------------------------------
//

#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <memory>
#include <concepts>
#include <bit>
#include "util.hpp"

#define CONSTEXPR

namespace automata {
	std::unordered_map<uintptr_t, std::unordered_set<void*>>& lazy_ptr_users() noexcept {
		static std::unordered_map<uintptr_t, std::unordered_set<void*>> users;
		return users;
	}
	template <typename T, typename Allocator = std::allocator<T>>
	struct lazy_ptr : protected Allocator {

	protected:
		uintptr_t p;

		template <typename...Args>
			requires std::constructible_from<T, Args...>
		CONSTEXPR T* ptr_alloc(Args&&...args) {
			T* p = (T*)Allocator::allocate(1);
			::new(p)T(std::forward<Args>(args)...);
			return p;
		}

		CONSTEXPR void ptr_free(T* p) {
			p->~T();
			Allocator::deallocate(p, 1);
		}

		CONSTEXPR T* ptr(T* s) noexcept {
			p = (uintptr_t)s | (p & OWNER_MASK);
			return ptr();
		}
		CONSTEXPR bool owner(bool b) noexcept {
			if (b) {
				p |= OWNER_MASK;
			} else {
				p &= ~OWNER_MASK;
			}
			return owner();
		}

		CONSTEXPR uintptr_t ptrint() const noexcept {
			return (uintptr_t)ptr();
		}

		static constexpr uintptr_t POINTER_MASK = pow2mask(bit_count<T*>() - 1);
		static constexpr uintptr_t OWNER_MASK = ~POINTER_MASK;

	public:
		CONSTEXPR T* ptr() noexcept {
			return (T*)(p & POINTER_MASK);
		}

		CONSTEXPR const T* ptr() const noexcept {
			return (const T*)(p & POINTER_MASK);
		}

		CONSTEXPR bool owner() const noexcept {
			return p & OWNER_MASK;
		}

		CONSTEXPR bool owns() const noexcept {
			return owner();
		}

		CONSTEXPR bool valid() const noexcept {
			return ptr() != nullptr;
		}

		CONSTEXPR bool lone_owner() const noexcept {
			return owns() && lazy_ptr_users()[ptrint()].size() == 0;
		}

		CONSTEXPR lazy_ptr() noexcept {
			ptr(nullptr);
			owner(false);
		}

		CONSTEXPR lazy_ptr(std::nullptr_t) noexcept : lazy_ptr() {}

		CONSTEXPR lazy_ptr(T* const&& p) noexcept : lazy_ptr() {
			ptr(p);
			owner((bool)p);
		}

		CONSTEXPR lazy_ptr& release() noexcept {
			if (!valid()) {
				return *this;
			}
			if (lone_owner()) {
				ptr_free(ptr());
				lazy_ptr_users().erase(ptrint());
			} else if (owns()) {
				auto* new_owner = (lazy_ptr<T>*)*lazy_ptr_users()[ptrint()].begin();
				new_owner->owner(true);
				lazy_ptr_users()[ptrint()].erase(new_owner);
			} else {
				lazy_ptr_users()[ptrint()].erase(this);
			}
			owner(false);
			ptr(nullptr);
			return *this;
		}

		CONSTEXPR lazy_ptr& operator=(std::nullptr_t) noexcept {
			return release();
		}

		CONSTEXPR lazy_ptr& use(const lazy_ptr& o) {
			if (std::addressof(o) == this) {
				return *this;
			}
			// insert first, for strong exception guarantee
			lazy_ptr_users()[o.ptrint()].insert(this);
			release();
			ptr(const_cast<T*>(o.ptr()));
			return *this;
		}

		CONSTEXPR lazy_ptr(const lazy_ptr& o) : lazy_ptr() {
			use(o);
		}

		CONSTEXPR lazy_ptr& own(lazy_ptr&& o) {
			if (std::addressof(o) == this || !o.valid()) {
				return *this;
			}
			release();
			if (o.owns()) {
				owner(true); 
			} else {
				lazy_ptr_users()[o.ptrint()].insert(this);
				lazy_ptr_users()[o.ptrint()].erase(std::addressof(o));
			}
			ptr(o.ptr());
			o.ptr(nullptr);
			o.owner(false);
			return *this;
		}

		CONSTEXPR lazy_ptr(lazy_ptr&& o) : lazy_ptr() {
			own(std::move(o));
		}

		CONSTEXPR lazy_ptr& operator=(const lazy_ptr& o) {
			return use(o);
		}

		CONSTEXPR lazy_ptr& operator=(lazy_ptr&& o) {
			return own(std::move(o));
		}

		CONSTEXPR ~lazy_ptr() noexcept {
			release();
		}

		CONSTEXPR operator bool() const noexcept {
			return valid();
		}

		CONSTEXPR const T* read() const noexcept {
			return ptr();
		}

		CONSTEXPR operator const T*() const noexcept {
			return read();
		}

		CONSTEXPR const T* operator->() const noexcept {
			return read();
		}

		CONSTEXPR const T& cref() const noexcept {
			return *read();
		}

		CONSTEXPR const T& operator*() const noexcept {
			return cref();
		}

		CONSTEXPR T* write() {
			// if this is the owner and |users[p]| != 0
			// then it needs a copy because a user might
			// want to use the object in the future
			//
			// if this is a user, then |users[p]| > 0
			// (this user is one of the users[p])
			if (!lone_owner()) {
				own(lazy_ptr(ptr_alloc(cref())));
			}
			return ptr();
		}

		CONSTEXPR T& ref() {
			return *write();
		}

		template <typename B, typename Ballocator, typename...Args>
		friend CONSTEXPR lazy_ptr<B, Ballocator> make_lazy(Args&&...args);
	};
	template <typename B, typename Ballocator = std::allocator<B>, typename...Args>
	CONSTEXPR lazy_ptr<B, Ballocator> make_lazy(Args&&...args) {
		lazy_ptr<B, Ballocator> result;
		result = lazy_ptr<B, Ballocator>(result.ptr_alloc(std::forward<Args>(args)...));
		return result;
	}
}

#endif // #ifndef AUTOMATA_LAZY_PTR_HPP
