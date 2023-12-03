import { Map } from "immutable";
import { DimensionGroup, Dimension, SqlFieldName } from "../../Api";
import { Lens } from "monocle-ts";

export type SplitGroup = { fieldGroup: DimensionGroup, selectedField: Dimension }

const _SelectedDimension: Lens<SplitGroup, Dimension> = Lens.fromProp<SplitGroup>()('selectedField');

export type ActiveSplits = Map<number, SplitGroup>;

/**
 * Insert a DimensionGroup into the map
 * @param afg The map of active fields
 * @param fieldGroup The field group that is to be inserted into the map
 * @returns The map `afg` with a new entry for the field group
 */
function addDimensionGroup(afg: ActiveSplits, fieldGroup: DimensionGroup): ActiveSplits {
    const maxIndex = afg.keySeq().max() || 0;
    const newIndex = maxIndex + 1;
    console.log("addDimensionGroup: " + newIndex.toString());
    const newGroup: SplitGroup = { fieldGroup, selectedField: fieldGroup.primary_dimension }
    return afg.set(newIndex, newGroup);
}

/**
 * Delete an entry from the map
 * @param afg The map of active fields
 * @param fieldGroupKey The key of the field that is to be deleted
 * @returns The map `afg` without the key
 */
function deleteDimensionGroup(afg: ActiveSplits, fieldGroupKey: number): ActiveSplits {
    console.log("Delete key: " + fieldGroupKey.toString());
    return afg.delete(fieldGroupKey);
}

/**
 * 
 * @param afg The map of active fields
 * @param fieldGroupKey The key of the field that is to be updated
 * @param dimension The new value of the 'InField'
 * @returns The map `afg` with they entry's inField changed to the new value
 */
function setSelectedField(afg: ActiveSplits, fieldGroupKey: number, dimension: Dimension): ActiveSplits {
    const oldValue = afg.get(fieldGroupKey);
    if (oldValue) {
        return afg.set(fieldGroupKey, _SelectedDimension.set(dimension)(oldValue))
    } else { return afg; }
}

/**
 * 
 * @param afg The map of active fields
 * @returns The selections with the specified parameters
 */
function splits(afg: ActiveSplits): SqlFieldName[] {
    return afg.toArray().map(x => x[1].selectedField.sql_field_name);
}

export default { addDimensionGroup, deleteDimensionGroup, setSelectedField, splits }