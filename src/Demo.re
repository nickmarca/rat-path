type coords = (int, int);

type matrix = array(array(int));

type direction =
  | Right
  | Down;

let goBottom = (i: matrix, (x, y): coords): coords => {
  i[x + 1][y] = i[x + 1][y] + 1;
  (x + 1, y);
};

let goUp = (i: matrix, (x, y): coords): coords => {
  i[x - 1][y] = i[x - 1][y] + 1;
  (x - 1, y);
};

let goRight = (i: matrix, (x, y): coords): coords => {
  i[x][y + 1] = i[x][y + 1] + 1;
  (x, y + 1);
};

let goLeft = (i: matrix, (x, y): coords): coords => {
  i[x][y - 1] = i[x][y - 1] + 1;
  (x, y - 1);
};

let caseBottom = (i: matrix, (x, y): coords): int =>
  x < 3 && i[x + 1][y] !== (-1) ? i[x + 1][y] : (-1);

let caseRight = (i: matrix, (x, y): coords): int =>
  y < 3 && i[x][y + 1] !== (-1) ? i[x][y + 1] : (-1);

let goBack = (i: matrix, (x, y): coords, lastMove: direction): coords => {
  switch (lastMove) {
  | Right => goLeft(i, (x, y))
  | Down => goUp(i, (x, y))
  };
};

let rec go =
        (i: matrix, (x, y): coords, mov: list(direction)): list(direction) =>
  if (x === 3 && y === 3) {
    mov;
  } else {
    let d1 = caseRight(i, (x, y));
    let d2 = caseBottom(i, (x, y));

    if (d1 === 0) {
      let newCoords = goRight(i, (x, y));
      go(i, newCoords, [Right, ...mov]);
    } else if (d2 === 0) {
      let newCoords = goBottom(i, (x, y));
      go(i, newCoords, [Down, ...mov]);
    } else {
      switch (mov) {
      | [m1, ...m2] =>
        let newCoords = goBack(i, (x, y), m1);
        go(i, newCoords, m2);
      | [] => []
      };
    };
  };

let input = Array.make_matrix(4, 4, 0);

input[0][0] = 1;
input[1][1] = (-1);
input[2][2] = (-1);
input[2][3] = (-1);

let output = Array.make_matrix(4, 4, 0);

output[0][0] = 1;

let res = go(input, (0, 0), []);

let rec printMov = l =>
  switch (l) {
  | [Right, ...rest] =>
    Js.log("Right");
    printMov(rest);
    ();
  | [Down, ...rest] =>
    Js.log("Down");
    printMov(rest);
    ();
  | [] => ()
  };

printMov(List.rev(res));