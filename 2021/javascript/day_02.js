const data = require('fs').readFileSync('../day_02.data', 'utf-8')

let [h, v] = data.split(/\r?\n/)
  .map(line => line.split(/ /))
  .map(([d, v]) => [d, Number(v)])
  .reduce(([h=0, v=0], [dir, val]) => {
    switch(dir) {
      case "forward": return [h+val, v];
      case "down": return [h, v+val]; 
      case "up": return [h, v-val];
    }
  }, []);

console.log(h * v);