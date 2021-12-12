var fs = require('fs');
var os = require('os');

var runnerPt1 = (input) => {
    console.log(">>>>" + input[0]);
    return input.length;
};

var loadInput = (inputFile) => fs
      .readFileSync(`${__dirname}/${inputFile}`)
      .toString()
      .split(os.EOL)
      .map((x) => x.trim().split(''));

/* scratchwork - this serves as a comment block that I can send expressions to the nodejs repl from,
 * but without evaluating when this file is loaded
 */
function scratch() {

    // loadInput("day10.txt");
    runnerPt1(loadInput("day10.txt"));

    runnerPt1([
        "[({(<(())[]>[[{[]{<()<>>",
        "[(()[<>])]({[<{<<[]>>(",
        "{([(<{}[<>[]}>{[]{[(<()>",
        "(((({<>}<{<{<>}{[]{[]{}",
        "[[<[([]))<([[{}[[()]]]",
        "[{[{({}]{}}([{[{{{}}([]",
        "{<[[]]>}<{[{[{[]{()[[[]",
        "[<(<(<(<{}))><([]([]()",
        "<{([([[(<>()){}]>(<<{{",
        "<{([{{}}[<[[[<>{}]]]>[]]"
    ]);
}


