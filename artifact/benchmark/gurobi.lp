\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
 
Subject To
 Cycle_nonempty: transitionVariable_n9 + transitionVariable_n10
   + transitionVariable_n11 + transitionVariable_n12
   + transitionVariable_n13 + transitionVariable_n14
   + transitionVariable_n15 + transitionVariable_n16
   + transitionVariable_n17 >= 1
 Effect_nonnegative_n1: - transitionVariable_n11 - transitionVariable_n14
   >= 0
 Effect_nonnegative_n2: transitionVariable_n9 + transitionVariable_n10
   >= 0
 Effect_nonnegative_n3: - transitionVariable_n9 - transitionVariable_n10
   + transitionVariable_n11 + transitionVariable_n14 >= 0
 Effect_nonnegative_n4: - transitionVariable_n9 - transitionVariable_n10
   + transitionVariable_n12 + transitionVariable_n13 >= 0
 Effect_nonnegative_n5: transitionVariable_n11 - transitionVariable_n12
   - transitionVariable_n13 + transitionVariable_n14 >= 0
 Effect_nonnegative_n6: transitionVariable_n13 - transitionVariable_n15
   - transitionVariable_n16 >= 0
 Effect_nonnegative_n7: transitionVariable_n14 - transitionVariable_n15
   - transitionVariable_n17 >= 0
 Effect_nonnegative_n8: - transitionVariable_n10 + transitionVariable_n15
   + transitionVariable_n16 + transitionVariable_n17 >= 0
Bounds
End
