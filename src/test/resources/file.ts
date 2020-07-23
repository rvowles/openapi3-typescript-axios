import { AeItemIntegerVO, AeItemStringVO, ObjectSerializer, Event } from './model';

// some testing against the sample

// const json = `{
//     "schema_version": "1",
//     "data": {
//         "errors": {
//             "account_holder": {
//                 "title": [
//                     "can't be blank"
//                 ],
//                 "name": [
//                     "can't be blank"
//                 ]
//             }
//         }
//     }
// }`;
//
// const data = JSON.parse(json);
//
// const de = ObjectSerializer.deserialize(data, 'SignupReplyError');
//
// console.log(JSON.stringify(de));
//
// const se = ObjectSerializer.serialize(de, 'SignupReplyError');
//
// console.log(JSON.stringify(se));
//
// const aeitemv0: any = {
// 	id: 36,
// 	textValue: 'text',
// 	item_type_id: 98,
// 	'@class': 'AeItemInteger',
// 	value: 15,
// };
//
// const aeDecoded = ObjectSerializer.deserialize(aeitemv0, 'AeItemVO');
// console.log(JSON.stringify(aeDecoded));
// console.log(aeDecoded instanceof AeItemIntegerVO);
// console.log(aeDecoded instanceof AeItemStringVO);
//
// console.log(JSON.stringify(ObjectSerializer.serialize(aeDecoded, 'AeItemVO')));

const evt: any = {
	id: 'id', img: 'img', someUniqueSet: [1, 2, 3]
};

const evtDecoded = ObjectSerializer.deserialize(evt, 'Event') as Event;
console.log('%j', evtDecoded, Array.from(evtDecoded.someUniqueSet));

console.log(ObjectSerializer.serialize(evtDecoded, 'Event'));
