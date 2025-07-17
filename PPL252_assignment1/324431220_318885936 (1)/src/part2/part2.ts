import * as R from "ramda";

const stringToArray = R.split("");

/* Question 1 */
const vowels: string[] = ['a', 'e', 'i', 'o', 'u'];
export const countVowels: (str: string) => number = (str: string): number =>
    R.pipe(stringToArray, R.map(R.toLower), R.filter((c: string) => vowels.includes(c)), R.length)(str);

/* Question 2 */
export const isPalindrome: (str: string) => boolean = (str: string): boolean => {
    const clean: string[] = R.pipe(R.toLower, R.replace(/[^a-z0-9]/gi, ""), stringToArray)(str);
    const reversed: string[] = R.pipe(stringToArray, (arr: string[]) => arr.reduce((acc, char) => [char, ...acc], [] as string[]))(clean.join(""));
    return R.equals(clean, reversed);
}
  
/* Question 3 */
export type WordTree = {
    root: string;
    children: WordTree[];
}

export const treeToSentence: (tree: WordTree) => string = (tree: WordTree): string => {
    const treeToArr: (node: WordTree) => string[] = (node: WordTree): string[] => [node.root, ...node.children.flatMap(treeToArr)];
    return R.join(" ", treeToArr(tree));
}
