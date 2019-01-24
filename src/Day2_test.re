open Jest;
open Expect;

describe("Day 2", () =>
  test("Part 1", () =>
    expect(Day2.solve()) |> toBe(5904)
  )
);