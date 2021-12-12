const assert = require('assert');
const day10 = require('./day10');

describe('Day 10: Syntax Scoring', () => {
  it('should sum the score of all corrupted input lines', () => {
    const input =
      `[({(<(())[]>[[{[]{<()<>>
       [(()[<>])]({[<{<<[]>>(
       {([(<{}[<>[]}>{[]{[(<()>
       (((({<>}<{<{<>}{[]{[]{}
       [[<[([]))<([[{}[[()]]]
       [{[{({}]{}}([{[{{{}}([]
       {<[[]]>}<{[{[{[]{()[[[]
       [<(<(<(<{}))><([]([]()
       <{([([[(<>()){}]>(<<{{
       <{([{{}}[<[[[<>{}]]]>[]]`;

    assert.strictEqual(day10.part1(input), 26397);
  });
});
