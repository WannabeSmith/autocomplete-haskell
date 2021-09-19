#include "HsFFI.h"
#ifdef __cplusplus
extern "C" {
#endif
extern HsPtr showC(HsStablePtr a1);
extern HsStablePtr emptyTrieC(void);
extern HsStablePtr rootNodeC(void);
extern HsStablePtr getNextNodeC(HsPtr a1, HsStablePtr a2);
extern HsStablePtr getNodeC(HsPtr a1, HsStablePtr a2);
extern HsInt32 isInTrieC(HsPtr a1, HsStablePtr a2);
extern HsDouble getMaxWeightC(HsStablePtr a1);
extern HsStablePtr updateMaxChildWeightC(HsStablePtr a1);
extern HsStablePtr insertChildC(HsStablePtr a1, HsStablePtr a2);
extern HsStablePtr insertPrefixC(HsDouble a1, HsPtr a2, HsStablePtr a3);
extern HsStablePtr insertWordsC(HsPtr a1, HsPtr a2, HsInt32 a3, HsStablePtr a4);
extern HsStablePtr trieFromListC(HsPtr a1, HsPtr a2, HsInt32 a3);
extern HsStablePtr removePrefixC(HsPtr a1, HsStablePtr a2);
extern HsStablePtr pruneTrieC(HsDouble a1, HsStablePtr a2);
extern HsStablePtr readTermsC(HsPtr a1);
extern void autocompleteByReferenceC(HsPtr a1, HsPtr a2, HsPtr a3, HsStablePtr a4, HsInt32 a5);
extern void slowCompleteByReferenceC(HsPtr a1, HsPtr a2, HsInt32 a3, HsPtr a4, HsPtr a5, HsPtr a6, HsInt32 a7);
#ifdef __cplusplus
}
#endif

