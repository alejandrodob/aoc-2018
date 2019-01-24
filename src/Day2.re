let inputFile = "src/day2-input.txt";

let hasAnyLetterExactly = (n, str) => {
  let asList = Utils.stringToCharList(str);
  let rec cont = (countMap, remaining) => {
    switch (remaining) {
    | [] => Utils.CharMap.exists((_, count) => count == n, countMap)
    | [head, ...tail] =>
      let currentCount =
        try (Utils.CharMap.find(head, countMap)) {
        | Not_found => 0
        };
      cont(Utils.CharMap.add(head, currentCount + 1, countMap), tail);
    };
  };
  cont(Utils.CharMap.empty, asList);
};

let numberOfIdsMatching = (n, ids) => {
  let getScore = id => hasAnyLetterExactly(n, id) ? 1 : 0;
  List.map(getScore, ids) |> List.fold_left((+), 0);
};

let calculateChecksum = ids =>
  numberOfIdsMatching(2, ids) * numberOfIdsMatching(3, ids);

let solve = () => inputFile |> Utils.inputFileToList |> calculateChecksum;