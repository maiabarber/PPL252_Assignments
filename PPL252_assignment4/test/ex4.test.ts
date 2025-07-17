import { expect, assert } from 'chai';
import { all, Fib1, Fib2 } from '../ex4';

//Q1
function p1() { // always succeeds, with content 1
  return new Promise((resolve, reject) => {
    setTimeout(() => { resolve(1); }, Math.random() * 1000);
  });
}

function p2() { // always succeeds, with content 2
  return new Promise((resolve, reject) => {
    setTimeout(() => { resolve(2); }, Math.random() * 1000);
  });
}

function p3() {  // always fails, with err 3
  return new Promise((resolve, reject) => {
    setTimeout(() => { reject(3); }, Math.random() * 1000);
  });
}

const take = <T>(n : number , generator : Iterator<T>) =>
  take1(n,generator, []);

const take1 = <T>(n : number , generator : Iterator<T>, acc : T[]) : T[] => {
    const ir = generator.next();
    if (n <= 0 || ir.done)
      return acc
    return take1(n-1,generator,acc.concat([ir.value]));
}

describe('Q1 Tests', () => {
    
    it("Q1 test 1", async () => {
        await all([p1(),p2()]).then(content => {
            expect(content).to.deep.equal([1,2])
        }).catch(err => {
            assert.fail("Q1 test 1 failed");
        })
    })

    it("Q1 test 2", async () => {
        await all([p1(),p3()]).then(content => {
            assert.fail("Q1 test 2 failed")
        }).catch(err => {
            assert(err === 3,"Q1 test 2 failed")
        })
    })
});

describe('Q2 Tests', () => {

    it("Q2 test 1", () => {
      expect(take(10, Fib1())).to.deep.equal([1,1,2,3,5,8,13,21,34,55]);
    })

    it("Q2 test 2", () => {
      expect(take(10, Fib2())).to.deep.equal([1,1,2,3,5,8,13,21,34,55]);
    })
});
