QUnit.test("Test Cookie", function (assert) {
  setCookie("TestCookie", "TestValue")
  assert.ok(getCookie("TestCookie") === "TestValue", "Passed!")
})
