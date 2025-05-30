#pragma once

#include <cstddef>
#include <stdexcept>
#include <algorithm>  // for std::copy, std::fill
#include <initializer_list>

namespace utl
{
  template <typename T>
  class Fix_container
  {
    T *data_;
    std::size_t size_;

  public:
    using value_type = T;
    using iterator = T *;
    using const_iterator = const T *;
    using reference = T &;
    using const_reference = const T &;
    using size_type = std::size_t;

    // Constructors
    Fix_container(std::size_t size) : data_(new T[size]{}), size_(size) {}

    Fix_container(std::initializer_list<T> init) : data_(new T[init.size()]), size_(init.size())
    {
      std::copy(init.begin(), init.end(), data_);
    }

    Fix_container(const Fix_container &other) : data_(new T[other.size_]), size_(other.size_)
    {
      std::copy(other.data_, other.data_ + size_, data_);
    }

    Fix_container &operator=(const Fix_container &other)
    {
      if (this != &other)
      {
        if (size_ != other.size_)
        {
          delete[] data_;
          size_ = other.size_;
          data_ = new T[size_];
        }
        std::copy(other.data_, other.data_ + size_, data_);
      }
      return *this;
    }

    Fix_container(Fix_container &&other) noexcept : data_(other.data_), size_(other.size_)
    {
      other.data_ = nullptr;
      other.size_ = 0;
    }

    Fix_container &operator=(Fix_container &&other) noexcept
    {
      if (this != &other)
      {
        delete[] data_;
        data_ = other.data_;
        size_ = other.size_;
        other.data_ = nullptr;
        other.size_ = 0;
      }
      return *this;
    }

    ~Fix_container() { delete[] data_; }

    // Access
    T &operator[](std::size_t index)
    {
      if (index >= size_)
        throw std::out_of_range("Fix_container index out of bounds");
      return data_[index];
    }

    T operator[](std::size_t index) const
    {
      if (index >= size_)
        throw std::out_of_range("Fix_container index out of bounds");
      return data_[index];
    }

    bool operator==(const Fix_container &other) const
    {
      if (size_ != other.size_)
        return false;
      for (std::size_t i = 0; i < size_; ++i)
        if (data_[i] != other.data_[i])
          return false;
      return true;
    }

    bool operator!=(const Fix_container &other) const { return !(*this == other); }

    T *data() { return data_; }
    const T *data() const { return data_; }

    std::size_t size() const { return size_; }
    bool empty() const { return size_ == 0; }

    // Iterators
    iterator begin() { return data_; }
    iterator end() { return data_ + size_; }
    const_iterator begin() const { return data_; }
    const_iterator end() const { return data_ + size_; }
    const_iterator cbegin() const { return data_; }
    const_iterator cend() const { return data_ + size_; }

    // Utility
    void fill(const T &value) { std::fill(data_, data_ + size_, value); }

    void swap(Fix_container &other) noexcept
    {
      std::swap(data_, other.data_);
      std::swap(size_, other.size_);
    }

    template <typename UnaryOp>
    void for_each(UnaryOp op)
    {
      for (std::size_t i = 0; i < size_; ++i) op(data_[i]);
    }

    template <typename UnaryOp>
    void transform(UnaryOp op)
    {
      for (std::size_t i = 0; i < size_; ++i) data_[i] = op(data_[i]);
    }

    template <typename Predicate>
    bool all_of(Predicate pred) const
    {
      for (std::size_t i = 0; i < size_; ++i)
        if (!pred(data_[i]))
          return false;
      return true;
    }

    template <typename Predicate>
    bool any_of(Predicate pred) const
    {
      for (std::size_t i = 0; i < size_; ++i)
        if (pred(data_[i]))
          return true;
      return false;
    }

    template <typename Predicate>
    T *find_if(Predicate pred)
    {
      for (std::size_t i = 0; i < size_; ++i)
        if (pred(data_[i]))
          return &data_[i];
      return nullptr;
    }

    template <typename Predicate>
    const T *find_if(Predicate pred) const
    {
      for (std::size_t i = 0; i < size_; ++i)
        if (pred(data_[i]))
          return &data_[i];
      return nullptr;
    }

    void iota(T start = T{})
    {
      for (std::size_t i = 0; i < size_; ++i) data_[i] = start++;
    }
  };

  template <typename T>
  void swap(Fix_container<T> &a, Fix_container<T> &b) noexcept
  {
    a.swap(b);
  }
}  // namespace utl
