import * as R from "../../src/lib/result";
import * as F from "../../src/part3/find";

describe("Assignment 1 - Part 3", () => {
    describe("findResult", () => {
        it("returns a Failure when no element was found", () => {
            const my_list: string[] = ["dog", "cat", "rat"]

            expect(F.findResult(x => x.length > 3, my_list)).toSatisfy(R.isFailure);
        });

        it("returns an Ok when an element was found", () => {
            const my_list: string[] = ["raccoon", "ostrich", "slug"]
            expect(F.findResult(x => x.length > 3, my_list)).toSatisfy(R.isOk);
        });
        
        // Your tests here (optional)
    });

    describe("returnSquaredIfFoundEven", () => {
        it("returns an Ok of the first even number squared in v2", () => {
            expect(F.returnSquaredIfFoundEven_v2([1, 2, 3])).toEqual(R.makeOk(4));
        });

        it("return a Failure if no even numbers are in the array in v2", () => {
            expect(F.returnSquaredIfFoundEven_v2([1, 3, 5])).toSatisfy(R.isFailure);
        });

        // Your tests here (optional)
    });
});