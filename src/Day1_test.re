open Jest;
open Expect;

describe("Day 1", () => {
  test("Part 1", () =>
    expect(Day1.solve1()) |> toBe(466)
  );

  test("Part 2", () =>
    expect(Day1.solve2()) |> toBe(750)
  );
});