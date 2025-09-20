# Phos Standard Library

This document lists all globally available functions and built-in methods for primitive types in Phos.

## Global Functions

---

### `len(collection)`
Returns the length or size of a collection.

-   **Parameters**:
    -   `collection` (`string` | `array`): The value to measure.
-   **Returns**: `i64`
    -   For a `string`, the number of characters.
    -   For an `array`, the number of elements.
-   **Example**:
    ```js
    let my_array := [10, 20, 30];
    print(len(my_array)); // Prints 3
    ```

### `clock()`
Returns the number of milliseconds that have passed since the Unix epoch.

-   **Parameters**: None
-   **Returns**: `f64`
-   **Example**:
    ```js
    let start_time := clock();
    ```

## Built-in Methods

---

### Array Methods

Array methods are called directly on an array variable (e.g., `my_array.push(1)`).

#### **Mutations (Modify the array)**

##### `push(element)`
Appends a new element to the end of the array. This function modifies the array in-place.
-   **Parameters**:
    -   `element` (`T`): The value to add. Its type must match the array's element type.
-   **Returns**: `void`
-   **Example**:
    ```js
    let nums: i64[] = [1];
    nums.push(2);
    print(nums); // Prints [1, 2]
    ```

##### `pop()`
Removes and returns the last element of the array. This modifies the array.
-   **Parameters**: None
-   **Returns**: `T` (The type of the array's elements)
-   **Example**:
    ```js
    let letters := ["a", "b", "c"];
    let last := letters.pop();
    print(last); // Prints "c"
    print(letters); // Prints ["a", "b"]
    ```

##### `insert(index: i64, element: T)`
Inserts an element at a given index, shifting subsequent elements to the right.
-   **Parameters**:
    -   `index` (`i64`): The position where the new element should be inserted.
    -   `element` (`T`): The value to add.
-   **Returns**: `void`
-   **Example**:
    ```js
    let nums := [10, 30];
    nums.insert(1, 20);
    print(nums); // Prints [10, 20, 30]
    ```

##### `remove(index: i64)`
Removes and returns the element at a given index, shifting subsequent elements to the left.
-   **Parameters**:
    -   `index` (`i64`): The position of the element to remove.
-   **Returns**: `T` (The removed element)
-   **Example**:
    ```js
    let nums := [10, 99, 20];
    let removed := nums.remove(1);
    print(removed); // Prints 99
    print(nums); // Prints [10, 20]
    ```

##### `clear()`
Removes all elements from the array.
-   **Parameters**: None
-   **Returns**: `void`
-   **Example**:
    ```js
    let nums := [1, 2, 3];
    nums.clear();
    print(nums); // Prints []
    ```

#### **Queries (Read information)**

##### `is_empty()`
Returns `true` if the collection has zero elements, `false` otherwise. This method is available on both `array` and `string`.
-   **Parameters**: None
-   **Returns**: `bool`
-   **Example**:
    ```js
    let empty_list: i64[] = [];
    print(empty_list.is_empty()); // Prints true
    ```

##### `contains(element: T)`
Returns `true` if the array contains an element that is equal to the given element.
-   **Parameters**:
    -   `element` (`T`): The value to search for.
-   **Returns**: `bool`
-   **Example**:
    ```js
    let nums := [10, 20, 30];
    print(nums.contains(20)); // Prints true
    ```

---

### String Methods

String methods are immutable. They do not modify the original string; instead, they return a new `string`.

##### `to_upper()`
Returns a new string with all alphabetic characters converted to uppercase.
-   **Parameters**: None
-   **Returns**: `string`
-   **Example**: `let upper := "hello".to_upper(); // "HELLO"`

##### `to_lower()`
Returns a new string with all alphabetic characters converted to lowercase.
-   **Parameters**: None
-   **Returns**: `string`
-   **Example**: `let lower := "WORLD".to_lower(); // "world"`

##### `trim()`
Returns a new string with leading and trailing whitespace removed.
-   **Parameters**: None
-   **Returns**: `string`
-   **Example**: `let clean := "  phos  ".trim(); // "phos"`

##### `split(delimiter: string)`
Returns an array of strings by splitting the original string at each occurrence of the delimiter.
-   **Parameters**:
    -   `delimiter` (`string`): The character sequence to split on.
-   **Returns**: `string[]`
-   **Example**: `let parts := "a,b,c".split(","); // ["a", "b", "c"]`

##### `replace(search: string, replacement: string)`
Returns a new string with all occurrences of the `search` string replaced by the `replacement` string.
-   **Parameters**:
    -   `search` (`string`): The substring to find.
    -   `replacement` (`string`): The string to substitute.
-   **Returns**: `string`
-   **Example**: `let new_str := "hello world".replace("world", "phos"); // "hello phos"`

### Optional Methods

These methods are available on any optional type (e.g., `string?`, `User?`). Let `T` be the base type of the optional.

| Method | Signature | Description |
| :--- | :--- | :--- |
| **`exists()`** | `() -> bool` | Returns `true` if the optional contains a value, `false` if it is `nil`. |
| **`value_or(default: T)`** | `(default: T) -> T` | Returns the contained value if one exists, otherwise returns the `default` value. |
| **`value()`** | `() -> T` | Returns the contained value. **Panics** with a runtime error if the optional is `nil`. |

**Example**:
```js
let maybe_name: string? = find_user(1).map(|u| u.name);
let display_name := maybe_name.value_or("Guest");
print(display_name);
```
