TEST_SET("Test the 'stack' object", {
    stack = new_stack()

    TEST(identical(stack$pop(), list()))

    stack$push("foo", "bar", "baz")
    TEST(identical(stack$pop(1), list("baz")))
    TEST(identical(stack$pop(), list("foo", "bar")))
    TEST(identical(stack$pop(), list()))
    })
