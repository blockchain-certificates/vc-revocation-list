/*!
 * Copyright (c) 2020-2022 Digital Bazaar, Inc. All rights reserved.
 */
import RevocationList from './RevocationList.js';

export async function decodeList({encodedList}) {
  return RevocationList.decode({encodedList});
}
