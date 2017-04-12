// Copyright (c) 2014 K Team. All Rights Reserved.

package org.kframework.builtin

import org.kframework.definition.ModuleName
import org.kframework.kore.ADT
import org.kframework.kore.KORE.Sort

object Sorts {
  val KString    = Sort("KString", ModuleName("KSTRING"))
  val String     = Sort("String", ModuleName("STRING-SYNTAX"))
  val Bool       = Sort("Bool", ModuleName("BOOL-SYNTAX"))
  val Int        = Sort("Int", ModuleName("INT-SYNTAX"))
  val Float      = Sort("Float", ModuleName("FLOAT-SYNTAX"))
  val K          = Sort("K", ModuleName("SORT-K"))
  val KBott      = Sort("KBott", ModuleName("BASIC-K"))
  val KVariable  = Sort("#KVariable", ModuleName("KSEQ-SYMBOLIC"))
  val KItem      = Sort("KItem", ModuleName("BASIC-K"))
  val KLabel     = Sort("KLabel", ModuleName("BASIC-K"))
  val KList      = Sort("KList", ModuleName("BASIC-K"))
  val KConfigVar = Sort("KConfigVar", ModuleName("BASIC-K"))
  val Id         = Sort("Id", ModuleName("ID"))
  val KResult    = Sort("KResult", ModuleName("BASIC-K"))

  val KSeq       = ADT.SortLookup("KSequence@???")
  val KBool      = Sort("KBool@???")
}
