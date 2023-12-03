import { Set } from "immutable";

export type ActiveMeasures = Set<string>

/**
 * 
 * @param afm The map of active measures
 * @param fieldKey The key of the measure that is to be updated
 * @returns The map `afm` with the entry's isActive field negated
 */
function toggleMeasure(afm: ActiveMeasures, fieldKey: string): ActiveMeasures {
    if (afm.has(fieldKey)) { return afm.delete(fieldKey)} else { return afm.add(fieldKey) }
}

export default { toggleMeasure }