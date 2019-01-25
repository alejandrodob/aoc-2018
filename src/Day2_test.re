open Jest;
open Expect;

describe("Day 2", () => {
  test("Part 1", () =>
    expect(Day2.solve1()) |> toBe(5904)
  );

  test("Part 2", () =>
    expect(Day2.solve2()) |> toBe("jiwamotgsfrudclzbyzkhlrvp")
  );
});