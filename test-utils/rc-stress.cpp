#include <cassert>
#include <iostream>
#include <string>
#include <vector>
#include "../src/memory/ref_counted.hpp"

using phos::mem::rc_ptr;

// Dummy class to track instances
struct Tracker
{
    static inline int alive = 0;
    std::string name;

    Tracker(std::string n) : name(std::move(n))
    {
        alive++;
        std::cout << "[CREATE] " << name << " (alive=" << alive << ")\n";
    }

    ~Tracker()
    {
        alive--;
        std::cout << "[DESTROY] " << name << " (alive=" << alive << ")\n";
    }
};

// A class that can hold a self-pointer (for cycle test)
struct Node
{
    rc_ptr<Node> next;
    int value;
    Node(int v) : value(v) { Tracker::alive++; }
    ~Node() { Tracker::alive--; }
};

void basic_copy_move_test()
{
    std::cout << "\n--- basic_copy_move_test ---\n";
    {
        rc_ptr<Tracker> p1(new Tracker("A"));
        assert(Tracker::alive == 1);

        rc_ptr<Tracker> p2 = p1;  // copy
        assert(Tracker::alive == 1);

        rc_ptr<Tracker> p3 = std::move(p2);  // move
        assert(Tracker::alive == 1);
        assert(!p2);  // moved-from
    }
    assert(Tracker::alive == 0);
}

void vector_stress_test()
{
    std::cout << "\n--- vector_stress_test ---\n";
    {
        std::vector<rc_ptr<Tracker>> vec;
        for (int i = 0; i < 1000; i++) vec.push_back(rc_ptr<Tracker>(new Tracker("T" + std::to_string(i))));
        assert(Tracker::alive == 1000);

        vec.clear();  // should free all
        assert(Tracker::alive == 0);
    }
}

void nested_scope_test()
{
    std::cout << "\n--- nested_scope_test ---\n";
    {
        rc_ptr<Tracker> outer;
        {
            rc_ptr<Tracker> inner(new Tracker("Scoped"));
            outer = inner;
            assert(Tracker::alive == 1);
        }
        // inner is gone, outer holds it alive
        assert(Tracker::alive == 1);
    }
    assert(Tracker::alive == 0);
}

void long_chain_test()
{
    std::cout << "\n--- long_chain_test ---\n";
    {
        rc_ptr<Node> head(new Node(0));
        rc_ptr<Node> cur = head;

        for (int i = 1; i < 10000; i++)
        {
            cur->next = rc_ptr<Node>(new Node(i));
            cur = cur->next;
        }

        assert(Tracker::alive == 10000);  // Node uses its own counter
        // If leaks exist, valgrind/sanitizer will show them.
    }
    assert(Tracker::alive == 0);  // Node uses its own counter
}

void cycle_test()
{
    std::cout << "\n--- cycle_test ---\n";
    {
        rc_ptr<Node> a(new Node(1));
        rc_ptr<Node> b(new Node(2));
        a->next = b;
        b->next = a;  // cycle created!

        // Both are still alive here
        assert(Tracker::alive == 2);
    }
    // Oops, cycle prevents cleanup!
    std::cout << "Alive after cycle: " << Tracker::alive << "\n";
    // expected: leak (intended) -> alive == 2
}

int main()
{
    basic_copy_move_test();
    vector_stress_test();
    nested_scope_test();
    long_chain_test();
    cycle_test();

    std::cout << "\nAll tests finished. Tracker::alive=" << Tracker::alive << "\n";
    return 0;
}
