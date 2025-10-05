#pragma once

#include <cstddef>
#include <utility>
#include <cassert>

namespace phos::mem
{

template <typename T>
class rc_ptr
{
    struct _control_block_
    {
        T *ptr;
        std::size_t ref_count;
        void (*deleter)(T*);   // function pointer to delete logic

        explicit _control_block_(T *p) : _control_block_(p, [](T *q) { delete q; }) {}
        explicit _control_block_(T *p, void (*d)(T *)) : ptr(p), ref_count(1), deleter(d) {}

        ~_control_block_()
        {
            if (ptr)
                deleter(ptr);
        }
    };

    _control_block_ *ctrl;

public:
    // Constructors
    rc_ptr() : ctrl(nullptr) {}
    explicit rc_ptr(T* raw) : ctrl(raw ? new _control_block_(raw, [](T* p){ delete p; }) : nullptr) {}
    rc_ptr(std::nullptr_t) noexcept : ctrl(nullptr) {}

    rc_ptr(const rc_ptr &other) noexcept : ctrl(other.ctrl)
    {
        if (ctrl)
            ++ctrl->ref_count;
    }

    rc_ptr(rc_ptr &&other) noexcept : ctrl(other.ctrl) { other.ctrl = nullptr; }

    // Assignment
    rc_ptr &operator=(const rc_ptr &other) noexcept
    {
        if (this != &other)
        {
            release();
            ctrl = other.ctrl;
            if (ctrl)
                ++ctrl->ref_count;
        }
        return *this;
    }

    rc_ptr &operator=(rc_ptr &&other) noexcept
    {
        if (this != &other)
        {
            release();
            ctrl = other.ctrl;
            other.ctrl = nullptr;
        }
        return *this;
    }

    // Destructor
    ~rc_ptr() { release(); }

    // Access
    T *get() const noexcept { return ctrl ? ctrl->ptr : nullptr; }
    T &operator*() const noexcept
    {
        assert(get());
        return *get();
    }
    T *operator->() const noexcept
    {
        assert(get());
        return get();
    }
    auto operator<=>(const rc_ptr&) const = default;

    // Utility
    std::size_t use_count() const noexcept { return ctrl ? ctrl->ref_count : 0; }
    bool unique() const noexcept { return use_count() == 1; }
    explicit operator bool() const noexcept { return get() != nullptr; }
    bool operator==(std::nullptr_t) const noexcept { return get() == nullptr; }

    void reset(T *new_ptr = nullptr)
    {
        if (ctrl)
        {
            if (--ctrl->ref_count == 0)
            {
                ctrl->deleter(ctrl->ptr);
                delete ctrl;
            }
            ctrl = nullptr;
        }

        if (new_ptr)
            ctrl = new _control_block_(new_ptr, [](T *p) { delete p; });
    }

private:
    void release()
    {
        if (ctrl && --ctrl->ref_count == 0)
        {
            delete ctrl;
            ctrl = nullptr;
        }
    }
};

template <typename T>
class rc_ptr<T[]>
{
    struct _control_block_
    {
        T* ptr;
        std::size_t ref_count;
        std::size_t sz;
        void (*deleter)(T*);

        explicit _control_block_(T* p, std::size_t n)
            : ptr(p), ref_count(1), sz(n), deleter([](T* q){ delete[] q; }) {}

        ~_control_block_()
        {
            if (ptr)
                deleter(ptr);
        }
    };

    _control_block_* ctrl;

public:
    // Constructors
    rc_ptr() : ctrl(nullptr) {}
    rc_ptr(T* raw, std::size_t n) 
        : ctrl(raw ? new _control_block_(raw, n) : nullptr) {}

    rc_ptr(const rc_ptr& other) noexcept : ctrl(other.ctrl)
    {
        if (ctrl) ++ctrl->ref_count;
    }

    rc_ptr(rc_ptr&& other) noexcept : ctrl(other.ctrl) { other.ctrl = nullptr; }

    // Assignment
    rc_ptr& operator=(const rc_ptr& other) noexcept
    {
        if (this != &other)
        {
            release();
            ctrl = other.ctrl;
            if (ctrl) ++ctrl->ref_count;
        }
        return *this;
    }

    rc_ptr& operator=(rc_ptr&& other) noexcept
    {
        if (this != &other)
        {
            release();
            ctrl = other.ctrl;
            other.ctrl = nullptr;
        }
        return *this;
    }

    // Destructor
    ~rc_ptr() { release(); }

    // Access
    T* get() const noexcept { return ctrl ? ctrl->ptr : nullptr; }
    T& operator[](std::size_t i) const
    {
        assert(ctrl && i < ctrl->sz && "Out of bounds");
        return ctrl->ptr[i];
    }

    std::size_t size() const noexcept { return ctrl ? ctrl->sz : 0; }

    std::size_t use_count() const noexcept { return ctrl ? ctrl->ref_count : 0; }
    bool unique() const noexcept { return use_count() == 1; }
    explicit operator bool() const noexcept { return get() != nullptr; }

private:
    void release()
    {
        if (ctrl && --ctrl->ref_count == 0)
        {
            delete ctrl;
            ctrl = nullptr;
        }
    }
};

template <typename T, typename... Args>
requires (!std::is_array_v<T>)
rc_ptr<T> make_rc(Args&&... args) {
    return rc_ptr<T>(new T(std::forward<Args>(args)...));
}

template <typename T>
requires std::is_array_v<T>
rc_ptr<T> make_rc(std::size_t n) {
    using Element = std::remove_extent_t<T>;
    return rc_ptr<T>(new Element[n], n);
}

}  // namespace phos::mem
