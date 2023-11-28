import { Map } from "immutable";
import Api, { FieldGroup, InField, OutField, Selections } from "../Api";
import { Lens } from "monocle-ts";

export type ActiveFieldGroup = { fieldGroup: FieldGroup, inField: InField, outField: OutField }

const _InField: Lens<ActiveFieldGroup, InField> = Lens.fromProp<ActiveFieldGroup>()('inField');
const _OutField: Lens<ActiveFieldGroup, OutField> = Lens.fromProp<ActiveFieldGroup>()('outField');

export type ActiveFields = Map<number, ActiveFieldGroup>;

/**
 * Insert a FieldGroup into the map
 * @param afg The map of active fields
 * @param fieldGroup The field group that is to be inserted into the map
 * @returns The map `afg` with a new entry for the field group
 */
function addFieldGroup(afg: ActiveFields, fieldGroup: FieldGroup): ActiveFields {
    const maxIndex = afg.keySeq().max() || 0;
    const newIndex = maxIndex + 1;
    console.log("addFieldGroup: " + newIndex.toString());
    const newGroup: ActiveFieldGroup = { fieldGroup, inField: Api.mkInField(fieldGroup.primary_field), outField: fieldGroup.primary_field }
    return afg.set(newIndex, newGroup);
}

/**
 * Delete an entry from the map
 * @param afg The map of active fields
 * @param fieldGroupKey The key of the field that is to be deleted
 * @returns The map `afg` without the key
 */
function deleteFieldGroup(afg: ActiveFields, fieldGroupKey: number): ActiveFields {
    console.log("Delete key: " + fieldGroupKey.toString());
    return afg.delete(fieldGroupKey);
}

/**
 * 
 * @param afg The map of active fields
 * @param fieldGroupKey The key of the field that is to be updated
 * @param inField The new value of the 'InField'
 * @returns The map `afg` with they entry's inField changed to the new value
 */
function setInField(afg: ActiveFields, fieldGroupKey: number, inField: InField): ActiveFields {
    const oldValue = afg.get(fieldGroupKey);
    if (oldValue) {
        return afg.set(fieldGroupKey, _InField.set(inField)(oldValue))
    } else { return afg; }
}

/**
 * 
 * @param afg The map of active fields
 * @param fieldGroupKey The key of the field that is to be updated
 * @param outField The new value of the 'OutField'
 * @returns The map `afg` with they entry's inField changed to the new value
 */
function setOutField(afg: ActiveFields, fieldGroupKey: number, outField: OutField): ActiveFields {
    const oldValue = afg.get(fieldGroupKey);
    if (oldValue) {
        return afg.set(fieldGroupKey, _OutField.set(outField)(_InField.set(Api.mkInField(outField))(oldValue)))
    } else { return afg; }
}

/**
 * 
 * @param afg The map of active fields
 * @returns The selections with the specified parameters
 */
function selections(afg: ActiveFields): Selections<InField> {
    const allFields: InField[] = afg.toArray().map(x => x[1].inField);
    return {
        _WildCards: allFields,
        _XAxis: [],
        _YAxis: [],
        _YAxis2: [],
        _Color: [],
        _selectedArchetype: [],
        _selectedMark: []
    }
    
}

export default { addFieldGroup, deleteFieldGroup, selections, setInField, setOutField }