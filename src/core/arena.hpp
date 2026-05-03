#ifndef ARENA_HPP_
#define ARENA_HPP_

#include <algorithm>
#include <cstddef>
#include <type_traits>
#include <vector>

namespace phos::mem {
class Arena
{
    static constexpr std::size_t DEF_BLOCK_SIZE = 6 * 1024;

    struct _block
    {
        char *data;
        std::size_t size;
        std::size_t capacity;

        _block(std::size_t sz) : size(0), capacity(sz)
        {
            data = new char[sz];
        }

        ~_block()
        {
            delete[] data;
        }
    };

    std::vector<_block *> blocks_;

    void add_block(std::size_t min_sz)
    {
        std::size_t block_size = std::max(this->DEF_BLOCK_SIZE, min_sz);
        this->blocks_.emplace_back(new _block(block_size));
    }

    inline std::size_t align_up(std::size_t value, std::size_t alignment)
    {
        return (value + alignment - 1) & ~(alignment - 1);
    }

public:
    ~Arena()
    {
        for (auto &block : blocks_) {
            delete block;
        }
        blocks_.clear();
    }

    Arena()
    {
        add_block(this->DEF_BLOCK_SIZE);
    }

    explicit Arena(std::size_t initial_size)
    {
        add_block(initial_size);
    }

    // No copying
    Arena(const Arena &) = delete;
    Arena &operator=(const Arena &) = delete;

    // Move support
    Arena(Arena &&other) noexcept : blocks_(std::move(other.blocks_))
    {
        other.blocks_.clear();
    }

    Arena &operator=(Arena &&other) noexcept
    {
        if (this != &other) {
            for (auto *b : blocks_) {
                delete b;
            }
            blocks_ = std::move(other.blocks_);
            other.blocks_.clear();
        }
        return *this;
    }

    template <typename T>
    T *allocate(std::size_t count = 1)
    {
        std::size_t bytes = sizeof(T) * count;
        std::size_t alignment = alignof(T);

        _block *back = this->blocks_.back();

        std::size_t start_offset = align_up(back->size, alignment);

        // 2. Check if we fit
        if (start_offset + bytes > back->capacity) {
            add_block(bytes + alignment); // Add space for alignment padding
            back = this->blocks_.back();
            start_offset = 0; // New block is always aligned
        }

        char *ptr = back->data + start_offset;
        back->size = start_offset + bytes;
        return reinterpret_cast<T *>(ptr);
    }

    void *allocate_bytes(std::size_t bytes, std::size_t alignment = 8)
    {
        _block *back = this->blocks_.back();

        // Align current offset to requested alignment
        std::size_t start_offset = align_up(back->size, alignment);

        if (start_offset + bytes > back->capacity) {
            add_block(bytes + alignment);
            back = this->blocks_.back();
            start_offset = 0;
        }

        char *ptr = back->data + start_offset;
        back->size = start_offset + bytes;
        return ptr;
    }

    template <typename T, typename... Args>
    T *create(Args &&...args)
    {
        T *mem = allocate<T>();
        return new (mem) T(std::forward<Args>(args)...);
    }

    void reset()
    {
        for (auto *b : blocks_) {
            b->size = 0;
        }
    }

    template <typename T, typename... Args>
    static T *alloc(Arena &arena, Args &&...args)
    {
        return arena.create<T>(std::forward<Args>(args)...);
    }

    template <typename T>
    static T *alloc(Arena &arena, T &&obj)
    {
        T *mem = arena.allocate<T>();
        return new (mem) T(std::forward<T>(obj));
    }

    template <typename T>
    T *operator<<(T &&obj)
    {
        using U = std::decay_t<T>;
        U *mem = this->allocate<U>();
        return new (mem) U(std::forward<T>(obj));
    }
};

} // namespace phos::mem
#endif // ARENA_HPP_
