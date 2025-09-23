#ifndef ARENA_HPP_
#define ARENA_HPP_
#include <algorithm>
#include <vector>
#include <cstddef>

namespace phos::mem
{
class Arena
{
    static constexpr std::size_t DEF_BLOCK_SIZE = 6 * 1024;

    struct _block
    {
        char *data;
        std::size_t size;
        std::size_t capacity;

        _block(std::size_t sz) : size(0), capacity(sz) { data = new char[sz]; }

        ~_block() { delete[] data; }
    };

    std::vector<_block *> blocks_;

    void add_block(std::size_t min_sz)
    {
        std::size_t block_size = std::max(this->DEF_BLOCK_SIZE, min_sz);
        this->blocks_.emplace_back(new _block(block_size));
    }

public:
    ~Arena()
    {
        for (auto &block : blocks_) delete block;
        blocks_.clear();
    }

    Arena() { add_block(this->DEF_BLOCK_SIZE); }

    Arena(const Arena &) = delete;
    Arena &operator=(const Arena &) = delete;

    Arena(Arena &&other) noexcept : blocks_(std::move(other.blocks_)) { other.blocks_.clear(); }

    Arena &operator=(Arena &&other) noexcept
    {
        if (this != &other)
        {
            for (auto *b : blocks_) delete b;
            blocks_ = std::move(other.blocks_);
            other.blocks_.clear();
        }
        return *this;
    }

    template <typename T>
    T *allocate(std::size_t count = 1)
    {
        std::size_t bytes = sizeof(T) * count;
        std::size_t aligned = (bytes + alignof(T) - 1) & ~(alignof(T) - 1);

        _block *back = this->blocks_.back();
        if (back->size + aligned > back->capacity)
        {
            add_block(aligned);
            back = this->blocks_.back();
        }

        char *ptr = back->data + back->size;
        back->size += aligned;
        return reinterpret_cast<T *>(ptr);
    }

    template <typename T, typename... Args>
    T *create(Args &&...args)
    {
        T *mem = allocate<T>();
        return new (mem) T(std::forward<Args>(args)...);
    }

    void reset()
    {
        for (auto *b : blocks_) b->size = 0;
    }

    // Static method for creating objects with constructor arguments
    template <typename T, typename... Args>
    static T *alloc(Arena &arena, Args &&...args)
    {
        return arena.create<T>(std::forward<Args>(args)...);
    }

    // Static method for move/copy constructing from an existing object
    template <typename T>
    static T *alloc(Arena &arena, T&& obj)
    {
        T *mem = arena.allocate<T>();
        return new (mem) T(std::forward<T>(obj));
    }

    // Operator for convenient object creation
    template <typename T>
    T *operator<<(T &&obj)
    {
        T *mem = this->allocate<std::decay_t<T>>();
        return new (mem) std::decay_t<T>(std::forward<T>(obj));
    }
};

}  // namespace phos::mem
#endif  // ARENA_HPP_
