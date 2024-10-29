import { throwInternalCompilerError } from "../errors";

export const eliminateLiteralUnion = <I, O>(
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