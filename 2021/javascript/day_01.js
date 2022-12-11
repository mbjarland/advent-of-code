const data = require('fs').readFileSync('../day_01.data', 'utf-8')

let [count] = data.split(/\r?\n/)
  .map(v => Number(v))
  .reduce(([count=0, prev], val) => 
    [count + (prev < val), val],
    []);

console.log(`increases: ${count}`);