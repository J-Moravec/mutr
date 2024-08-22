source("../src/stack.r")

TEST_SET("Test the 'stack' object", {
    stack = new_stack()

    TEST(identical(stack$peek(), list()))
    TEST(identical(stack$pop(), list()))

    stack$push("foo", "bar", "baz")
    TEST(identical(stack$peek(), list("foo", "bar", "baz")))
    TEST(identical(stack$peek(1), list("baz")))
    TEST(identical(stack$pop(1), list("baz")))
    TEST(identical(stack$pop(), list("foo", "bar")))
    TEST(identical(stack$pop(), list()))
    })
