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

let solve1 = () => inputFile |> Utils.inputFileToList |> calculateChecksum;

let hammingDistance = (s1, s2) => {
  List.fold_left2(
    (acc, i1, i2) => i1 == i2 ? acc : acc + 1,
    0,
    Utils.stringToCharList(s1),
    Utils.stringToCharList(s2),
  );
};

let arePrototypeBoxes = (id1, id2) => hammingDistance(id1, id2) == 1;

let rec findCommonInList = (l1, l2) => {
  switch ([l1, l2]) {
  | [[], []] => []
  | [[h1, ...t1], [h2, ...t2]] =>
    h1 == h2 ? [h1, ...findCommonInList(t1, t2)] : findCommonInList(t1, t2)
  | _ => raise(Invalid_argument("Lists length do not match"))
  };
};

let findCommonChars = (s1, s2) =>
  Utils.charListToString(
    findCommonInList(
      Utils.stringToCharList(s1),
      Utils.stringToCharList(s2),
    ),
  );

let findOpt = (predicate, xs) =>
  try (Some(List.find(predicate, xs))) {
  | Not_found => None
  };

let rec findPrototypeBoxes = ids =>
  switch (ids) {
  | [] => None
  | [head, ...tail] =>
    switch (findOpt(id => arePrototypeBoxes(head, id), ids)) {
    | None => findPrototypeBoxes(tail)
    | Some(id) => Some((head, id))
    }
  };

let checkIds = ids =>
  switch (findPrototypeBoxes(ids)) {
  | None => ""
  | Some((id1, id2)) => findCommonChars(id1, id2)
  };

let solve2 = () => inputFile |> Utils.inputFileToList |> checkIds;