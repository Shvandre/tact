import { throwInternalCompilerError } from "../errors";

/**
 * Convert union to intersection. See https://stackoverflow.com/q/50374908
 */
export type Intersect<T> = (T extends unknown ? ((x: T) => 0) : never) extends ((x: infer R) => 0) ? R : never

/**
 * Makes types more readable
 * Example: Unwrap<{ a: 1 } & { b: 2 }> = { a: 1, b: 2 }
 */
export type Unwrap<T> = T extends infer R ? {[K in keyof R]: R[K]} : never;

/**
 * Make visitor for literal union
 */
export const makeLiteralVisitor = <I, O>(
    handlers: { [K in keyof I]: () => O }
) => (input: Extract<keyof I, string>): O => {
    const handler = handlers[input];

    // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
    if (handler) {
        return handler();
    } else {
        throwInternalCompilerError(`Reached impossible case: ${input}`);
    }
};

type Inputs<I> = I extends { kind: infer K } ? K extends string ? Record<K, (input: I) => unknown> : never : never
type Outputs<O> = { [K in keyof O]: (input: never) => O[K] }
type Handlers<I, O> = Unwrap<Intersect<Inputs<I>>> & Outputs<O>

/**
 * Make visitor for disjoint union (tagged union, discriminated union)
 */
export const makeVisitor = <I>() => <O>(handlers: Handlers<I, O>) => {
    return (input: Extract<I, { kind: string }>): O[keyof O] => {
        const handler = (handlers as Record<string, (input: I) => O[keyof O]>)[input.kind];

        // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
        if (handler) {
            return handler(input);
        } else {
            throwInternalCompilerError(`Reached impossible case: ${input.kind}`);
        }
    };
};
