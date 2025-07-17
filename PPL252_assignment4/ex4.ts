//Q1
export function all<T>(promises : Array<Promise<T>>) : Promise<Array<T>> {

  return new Promise((resolve, reject) => {
    const results: T[] = [];
    let completed = 0;

    if (promises.length === 0) {
      resolve([]);
      return;
    }

    promises.forEach((promise, index) => {
      promise.then(value => {
        results[index] = value;
        completed++;
        if (completed === promises.length) {
          resolve(results);
        }
      }).catch(error => {
        reject(error);
      });
    });
  });
}

  
// Q2
export function* Fib1() {
  let a = 1, b = 1;
  yield a; // F1
  yield b; // F2
  while (true) {
    const next = a + b;
    yield next;
    a = b;
    b = next;
  }
}


export function* Fib2() {
  let n = 1;
  const sqrt5 = Math.sqrt(5);
  const phi = (1 + sqrt5) / 2;
  const psi = (1 - sqrt5) / 2;

  while (true) {
    const Fn = (Math.pow(phi, n) - Math.pow(psi, n)) / sqrt5;
    yield Math.round(Fn); // Round to account for floating point precision
    n++;
  }
}