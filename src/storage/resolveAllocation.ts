import type { CompilerContext } from "@/context/context";
import { createContextStore } from "@/context/context";
import { getType, toBounced, getAllTypes } from "@/types/resolveDescriptors";
import type { TypeDescription } from "@/types/types";
import { topologicalSort } from "@/utils/utils";
import type { StorageAllocation } from "@/storage/StorageAllocation";
import type { AllocationOperation } from "@/storage/operation";
import { allocate, getAllocationOperationFromField } from "@/storage/allocator";
import {
    createABITypeRefFromTypeRef,
    resolveABIType,
} from "@/types/resolveABITypeRef";
import { funcInitIdOf } from "@/generator/writers/id";
import { throwInternalCompilerError } from "@/error/errors";

const store = createContextStore<StorageAllocation>();

export function getAllocation(
    ctx: CompilerContext,
    name: string,
): StorageAllocation {
    const t = store.get(ctx, name);
    if (!t) {
        throwInternalCompilerError(`Allocation for ${name} not found`);
    }
    return t;
}

export function getAllocations(ctx: CompilerContext): {
    allocation: StorageAllocation;
    type: TypeDescription;
}[] {
    return getSortedTypes(ctx).map((v) => ({
        allocation: getAllocation(ctx, v.name),
        type: v,
    }));
}

export function getSortedTypes(ctx: CompilerContext): TypeDescription[] {
    const types = getAllTypes(ctx).filter(
        (v) => v.kind === "struct" || v.kind === "contract",
    );
    let structs = types.filter((t) => t.kind === "struct");
    const refs = (src: TypeDescription) => {
        const res: TypeDescription[] = [];
        const t: Set<string> = new Set();
        for (const f of src.fields) {
            const r = f.type;
            if (r.kind === "ref") {
                const tp = getType(ctx, r.name);
                if (tp.kind === "struct") {
                    if (!t.has(tp.name)) {
                        t.add(r.name);
                        res.push(tp);
                    }
                }
            }
        }
        return res;
    };
    structs = topologicalSort(structs, refs);
    structs = [...structs, ...types.filter((v) => v.kind === "contract")];
    return structs;
}

export function resolveAllocations(ctx: CompilerContext): CompilerContext {
    // Load topological order of structs and contracts
    const types = getSortedTypes(ctx);

    // Generate allocations
    for (const s of types) {
        // Reserve bits
        let reserveBits = 0;
        let header: { value: number; bits: number } | null = null;
        if (s.header !== null) {
            reserveBits += 32; // Header size
            header = { value: Number(s.header.value), bits: 32 };
        }

        // Reserver refs
        let reserveRefs = 0;
        if (s.kind === "contract") {
            reserveRefs += 1; // Internal state
        }

        // Convert fields
        const ops: AllocationOperation[] = [];
        const partialOps: AllocationOperation[] = [];
        for (const [i, f] of s.fields.entries()) {
            const op = {
                name: f.name,
                type: f.abi.type,
                op: getAllocationOperationFromField(
                    f.abi.type,
                    (name) => getAllocation(ctx, name)!.size,
                ),
            };
            ops.push(op);
            if (i < s.partialFieldCount) {
                partialOps.push(op);
            }
        }

        // Perform allocation
        const root = allocate({
            ops,
            reserved: { bits: reserveBits, refs: reserveRefs },
        });
        const partialRoot = allocate({
            ops: partialOps,
            reserved: { bits: reserveBits, refs: reserveRefs },
        });

        // Store allocation
        const allocation: StorageAllocation = {
            ops,
            root,
            header,
            size: {
                bits: root.size.bits + reserveBits,
                refs: root.size.refs + reserveRefs,
            },
        };

        const partialAllocation: StorageAllocation = {
            ops: partialOps,
            root: partialRoot,
            header,
            size: {
                bits: root.size.bits + reserveBits,
                refs: root.size.refs + reserveRefs,
            },
        };

        ctx = store.set(ctx, s.name, allocation);
        ctx = store.set(ctx, toBounced(s.name), partialAllocation);
    }

    // Generate init allocations
    for (const s of types) {
        if (s.kind === "contract" && s.init) {
            // Reserve bits and refs
            let reserveBits = 0;
            let reserveRefs = 0;

            // Reserve first bit for init state
            reserveBits++;

            // Reserve ref for system cell
            reserveRefs++;

            // Resolve opts
            const ops: AllocationOperation[] = [];
            if (s.init.kind !== "contract-params") {
                for (const f of s.init.params) {
                    const abiType = createABITypeRefFromTypeRef(
                        ctx,
                        f.type,
                        f.loc,
                    );
                    ops.push({
                        name: f.name.kind === "id" ? f.name.text : "_",
                        type: abiType,
                        op: getAllocationOperationFromField(
                            abiType,
                            (name) => getAllocation(ctx, name)!.size,
                        ),
                    });
                }
            } else {
                for (const f of s.init.contract.params ?? []) {
                    const abiType = resolveABIType(f);
                    ops.push({
                        name: f.name.text,
                        type: abiType,
                        op: getAllocationOperationFromField(
                            abiType,
                            (name) => getAllocation(ctx, name)!.size,
                        ),
                    });
                }
            }

            // Perform allocation
            const root = allocate({
                ops,
                reserved: { bits: reserveBits, refs: reserveRefs },
            }); // Better allocation?

            // Store allocation
            const allocation: StorageAllocation = {
                ops,
                root,
                header: null,
                size: {
                    bits: root.size.bits + reserveBits,
                    refs: root.size.refs + reserveRefs,
                },
            };
            ctx = store.set(ctx, funcInitIdOf(s.name), allocation);
        }
    }

    return ctx;
}
