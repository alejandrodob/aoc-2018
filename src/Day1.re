let day1Input = "src/day1-input.txt";

let parseFrequencies = frequencies => List.map(int_of_string, frequencies);

let sumFrequencies = frequencies => List.fold_left((+), 0, frequencies);

let solve1 = () =>
  Utils.inputFileToList(day1Input) |> parseFrequencies |> sumFrequencies;

let rec findFirstDuplicateSum =
        (~sums=Utils.IntSet.empty, ~lastSum=0, ~remaining=[], list) => {
  switch (remaining) {
  | [] => findFirstDuplicateSum(~sums, ~lastSum, ~remaining=list, list)
  | [head, ...tail] =>
    let nextSum = lastSum + head;
    Utils.IntSet.mem(nextSum, sums) ?
      nextSum :
      findFirstDuplicateSum(
        ~sums=Utils.IntSet.add(nextSum, sums),
        ~lastSum=nextSum,
        ~remaining=tail,
        list,
      );
  };
};

let solve2 = () => {
  Utils.inputFileToList(day1Input)
  |> parseFrequencies
  |> findFirstDuplicateSum;
};

let generateSumsStream = frequencies => {
  Utils.Stream.(frequencies |> ofList |> cycle |> scanLeft((+), 0));
};

let findFirstDuplicate = stream => {
  open Utils.Stream;
  let rec findDup = (visited, toVisit) =>
    switch (hd(toVisit)) {
    | None => None
    | Some(a) =>
      Utils.IntSet.mem(a, visited) ?
        Some(a) : findDup(Utils.IntSet.add(a, visited), tl(toVisit))
    };

  findDup(Utils.IntSet.empty, stream);
};

let solve2Streams = () => {
  Utils.inputFileToList(day1Input)
  |> parseFrequencies
  |> generateSumsStream
  |> findFirstDuplicate;
};