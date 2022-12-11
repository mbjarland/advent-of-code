const data = require("fs").readFileSync("../day_03.data", "utf-8");

let cols    = transpose(data.split(/\r?\n/))
let gamma   = getRate(cols, (a, b) => a < b)
let epsilon = getRate(cols, (a, b) => a >= b)

console.log(parseInt(gamma, 2) * parseInt(epsilon, 2));

function getRate(cols, op) {
  return cols.map( col => {
    const zeros = col.split('0').length - 1;
    const ones  = col.length - zeros
    return op(zeros, ones) ? 0 : 1;
  }).join('');
}

function transpose(lines) {
  const arr = line => [...line];
  return arr(lines[0]).map((_, colIndex) => 
    lines.map(line => line[colIndex]).join('')
  );
}
