#include <iostream>
#include <cassert>

#include "../src/memory/ref_counted.hpp"

struct Tracker
{
    static inline int live_count = 0;
    int id;

    Tracker(int id) : id(id)
    {
        ++live_count;
        std::cout << "Tracker(" << id << ") constructed\n";
    }

    ~Tracker()
    {
        --live_count;
        std::cout << "Tracker(" << id << ") destroyed\n";
    }
};

// ------------------------------
// Test cases
// ------------------------------

void test_basic_construction()
{
    std::cout << "\n--- test_basic_construction ---\n";
    {
        phos::mem::rc_ptr<Tracker> a = phos::mem::make_rc<Tracker>(1);
        assert(a.use_count() == 1);
        assert(Tracker::live_count == 1);
        assert(a->id == 1);
    }
    assert(Tracker::live_count == 0);  // destroyed
}

void test_copy_semantics()
{
    std::cout << "\n--- test_copy_semantics ---\n";
    {
        phos::mem::rc_ptr<Tracker> a = phos::mem::make_rc<Tracker>(2);
        assert(a.use_count() == 1);

        phos::mem::rc_ptr<Tracker> b = a;  // copy
        assert(a.use_count() == 2);
        assert(b.use_count() == 2);

        phos::mem::rc_ptr<Tracker> c(a);  // copy constructor
        assert(a.use_count() == 3);
        assert(b.use_count() == 3);
        assert(c.use_count() == 3);

        assert(Tracker::live_count == 1);
    }
    assert(Tracker::live_count == 0);  // all destroyed
}

void test_move_semantics()
{
    std::cout << "\n--- test_move_semantics ---\n";
    {
        phos::mem::rc_ptr<Tracker> a = phos::mem::make_rc<Tracker>(3);
        assert(a.use_count() == 1);

        phos::mem::rc_ptr<Tracker> b = std::move(a);
        assert(!a);  // moved-from should be empty
        assert(b.use_count() == 1);
        assert(Tracker::live_count == 1);

        phos::mem::rc_ptr<Tracker> c(std::move(b));
        assert(!b);
        assert(c.use_count() == 1);
        assert(Tracker::live_count == 1);
    }
    assert(Tracker::live_count == 0);
}

void test_assignment()
{
    std::cout << "\n--- test_assignment ---\n";
    {
        phos::mem::rc_ptr<Tracker> a = phos::mem::make_rc<Tracker>(4);
        phos::mem::rc_ptr<Tracker> b = phos::mem::make_rc<Tracker>(5);

        assert(a.use_count() == 1);
        assert(b.use_count() == 1);
        assert(Tracker::live_count == 2);

        b = a;  // copy assignment
        assert(a.use_count() == 2);
        assert(b.use_count() == 2);
        assert(Tracker::live_count == 1);  // Tracker(5) destroyed
    }
    assert(Tracker::live_count == 0);
}

void test_reset()
{
    std::cout << "\n--- test_reset ---\n";
    {
        phos::mem::rc_ptr<Tracker> a = phos::mem::make_rc<Tracker>(6);
        assert(a.use_count() == 1);

        a.reset(new Tracker(7));
        assert(a.use_count() == 1);
        assert(a->id == 7);
        assert(Tracker::live_count == 1);
    }
    assert(Tracker::live_count == 0);
}

void test_unique()
{
    std::cout << "\n--- test_unique ---\n";
    {
        phos::mem::rc_ptr<Tracker> a = phos::mem::make_rc<Tracker>(8);
        assert(a.unique());

        phos::mem::rc_ptr<Tracker> b = a;
        assert(!a.unique());
        assert(!b.unique());

        b.reset();
        assert(a.unique());
    }
    assert(Tracker::live_count == 0);
}

void test_null_rc()
{
    std::cout << "\n--- test_null_rc ---\n";
    {
        phos::mem::rc_ptr<Tracker> a;  // empty
        assert(!a);
        assert(a.use_count() == 0);

        phos::mem::rc_ptr<Tracker> b = a;
        assert(!b);
        assert(b.use_count() == 0);

        phos::mem::rc_ptr<Tracker> c = std::move(a);
        assert(!c);
        assert(c.use_count() == 0);
    }
}

int main()
{
    test_basic_construction();
    test_copy_semantics();
    test_move_semantics();
    test_assignment();
    test_reset();
    test_unique();
    test_null_rc();

    std::cout << "\nAll tests passed!\n";
}
