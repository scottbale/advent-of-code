var fs = require('fs');
var os = require('os');
var _ = require('underscore');

var runnerPt1 = (input) => {
    console.log(">>>>first line of input " + input[0]);
    return input.length;
};

var unpackInput = (rawInput) => rawInput.split(os.EOL).map((x) => x.trim().split(''));

var loadInput = (inputFile) => fs
      .readFileSync(`${__dirname}/${inputFile}`)
      .toString();

module.exports = {
    part1: _.compose(runnerPt1, unpackInput)
};

/* scratchwork - this serves as a comment block that I can send expressions to the nodejs repl from,
 * but without evaluating when this file is loaded
 */
function scratch() {

    runnerPt1(unpackInput(loadInput("day10.txt")));

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

    runnerPt1(unpackInput(
      `[({(<(())[]>[[{[]{<()<>>
       [(()[<>])]({[<{<<[]>>(
       {([(<{}[<>[]}>{[]{[(<()>
       (((({<>}<{<{<>}{[]{[]{}
       [[<[([]))<([[{}[[()]]]
       [{[{({}]{}}([{[{{{}}([]
       {<[[]]>}<{[{[{[]{()[[[]
       [<(<(<(<{}))><([]([]()
       <{([([[(<>()){}]>(<<{{
       <{([{{}}[<[[[<>{}]]]>[]]`
    ));


}


