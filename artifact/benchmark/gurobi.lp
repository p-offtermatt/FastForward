\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
 
Subject To
 Cycle_nonempty:
   transitionVariable_callToProcess.s00005925.inputCriterion.s00000937
   + transitionVariable_callToProcess.s00005925.outputCriterion.s00000939
   + transitionVariable_callToTask.s00001435.inputCriterion.s00000937
   + transitionVariable_callToTask.s00001435.outputCriterion.s00000939
   + transitionVariable_callToTask.s00001436.inputCriterion.s00000937
   + transitionVariable_callToTask.s00001436.outputCriterion.s00000939
   + transitionVariable_callToTask.s00003493.inputCriterion.s00000937
   + transitionVariable_callToTask.s00003493.outputCriterion.s00000939
   + transitionVariable_callToTask.s00005844.inputCriterion.s00000937
   + transitionVariable_callToTask.s00005844.outputCriterion.s00000939
   + transitionVariable_callToTask.s00005848.inputCriterion.s00000937
   + transitionVariable_callToTask.s00005848.outputCriterion.s00000939
   + transitionVariable_callToTask.s00006283.inputCriterion.s00000937
   + transitionVariable_callToTask.s00006283.outputCriterion.s00000939
   + transitionVariable_callToTask.s00006284.inputCriterion.s00000937
   + transitionVariable_callToTask.s00006284.outputCriterion.s00000939
   + transitionVariable_callToTask.s00006285.inputCriterion.s00000937
   + transitionVariable_callToTask.s00006285.outputCriterion.s00000939
   + transitionVariable_callToTask.s00006286.inputCriterion.s00000937
   + transitionVariable_callToTask.s00006286.outputCriterion.s00000939
   + transitionVariable_callToTask.s00006287.inputCriterion.s00000937
   + transitionVariable_callToTask.s00006287.outputCriterion.s00000939
   + transitionVariable_callToTask.s00006288.inputCriterion.s00000937
   + transitionVariable_callToTask.s00006288.outputCriterion.s00000939
   + transitionVariable_process.s00000357##s00006282.inputCriterion.s00000937
   + transitionVariable_process.s00000357##s00006282.outputCriterion.s00000939
   >= 1
 Effect_nonnegative_alpha:
   - transitionVariable_process.s00000357##s00006282.inputCriterion.s00000937
   >= 0
 Effect_nonnegative_callToProcess.s00005925.inputCriterion.s00000937.used:
   transitionVariable_callToProcess.s00005925.inputCriterion.s00000937
   - transitionVariable_callToProcess.s00005925.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToProcess.s00005925.output.s00000938:
   transitionVariable_callToProcess.s00005925.outputCriterion.s00000939
   - transitionVariable_callToTask.s00005844.inputCriterion.s00000937 >= 0
 Effect_nonnegative_callToProcess.s00005925.output.s00000959:
   transitionVariable_callToProcess.s00005925.outputCriterion.s00000939
   - transitionVariable_callToTask.s00005844.inputCriterion.s00000937 >= 0
 Effect_nonnegative_callToProcess.s00005925.output.s00000979:
   transitionVariable_callToProcess.s00005925.outputCriterion.s00000939
   - transitionVariable_callToTask.s00005844.inputCriterion.s00000937 >= 0
 Effect_nonnegative_callToTask.s00001435.input.s00000986:
   - transitionVariable_callToTask.s00001435.inputCriterion.s00000937
   + transitionVariable_process.s00000357##s00006282.inputCriterion.s00000937
   >= 0
 Effect_nonnegative_callToTask.s00001435.inputCriterion.s00000937.used:
   transitionVariable_callToTask.s00001435.inputCriterion.s00000937
   - transitionVariable_callToTask.s00001435.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00001435.output.s00000956:
   transitionVariable_callToTask.s00001435.outputCriterion.s00000939
   - transitionVariable_callToTask.s00001436.inputCriterion.s00000937 >= 0
 Effect_nonnegative_callToTask.s00001435.output.s00000959:
   transitionVariable_callToTask.s00001435.outputCriterion.s00000939
   - transitionVariable_callToTask.s00001436.inputCriterion.s00000937 >= 0
 Effect_nonnegative_callToTask.s00001435.output.s00000979:
   transitionVariable_callToTask.s00001435.outputCriterion.s00000939
   - transitionVariable_callToTask.s00001436.inputCriterion.s00000937 >= 0
 Effect_nonnegative_callToTask.s00001436.inputCriterion.s00000937.used:
   transitionVariable_callToTask.s00001436.inputCriterion.s00000937
   - transitionVariable_callToTask.s00001436.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00001436.output.s00000938:
   - transitionVariable_callToProcess.s00005925.inputCriterion.s00000937
   + transitionVariable_callToTask.s00001436.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00001436.output.s00000979:
   - transitionVariable_callToProcess.s00005925.inputCriterion.s00000937
   + transitionVariable_callToTask.s00001436.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00001436.output.s00000981:
   - transitionVariable_callToProcess.s00005925.inputCriterion.s00000937
   + transitionVariable_callToTask.s00001436.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00003493.inputCriterion.s00000937.used:
   transitionVariable_callToTask.s00003493.inputCriterion.s00000937
   - transitionVariable_callToTask.s00003493.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00003493.output.s00000938:
   transitionVariable_callToTask.s00003493.outputCriterion.s00000939
   - transitionVariable_process.s00000357##s00006282.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00003493.output.s00000956:
   transitionVariable_callToTask.s00003493.outputCriterion.s00000939
   - transitionVariable_process.s00000357##s00006282.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00003493.output.s00000960:
   transitionVariable_callToTask.s00003493.outputCriterion.s00000939 >= 0
 Effect_nonnegative_callToTask.s00003493.output.s00000979:
   transitionVariable_callToTask.s00003493.outputCriterion.s00000939
   - transitionVariable_process.s00000357##s00006282.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00005844.inputCriterion.s00000937.used:
   transitionVariable_callToTask.s00005844.inputCriterion.s00000937
   - transitionVariable_callToTask.s00005844.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00005844.output.s00000938:
   transitionVariable_callToTask.s00005844.outputCriterion.s00000939
   - transitionVariable_callToTask.s00006287.inputCriterion.s00000937 >= 0
 Effect_nonnegative_callToTask.s00005844.output.s00000956:
   transitionVariable_callToTask.s00005844.outputCriterion.s00000939
   - transitionVariable_callToTask.s00006285.inputCriterion.s00000937 >= 0
 Effect_nonnegative_callToTask.s00005844.output.s00000959:
   transitionVariable_callToTask.s00005844.outputCriterion.s00000939
   - transitionVariable_callToTask.s00006284.inputCriterion.s00000937 >= 0
 Effect_nonnegative_callToTask.s00005844.output.s00000960:
   transitionVariable_callToTask.s00005844.outputCriterion.s00000939
   - transitionVariable_callToTask.s00006286.inputCriterion.s00000937 >= 0
 Effect_nonnegative_callToTask.s00005844.output.s00000961:
   transitionVariable_callToTask.s00005844.outputCriterion.s00000939
   - transitionVariable_callToTask.s00006288.inputCriterion.s00000937 >= 0
 Effect_nonnegative_callToTask.s00005844.output.s00000962:
   transitionVariable_callToTask.s00005844.outputCriterion.s00000939
   - transitionVariable_callToTask.s00006284.inputCriterion.s00000937 >= 0
 Effect_nonnegative_callToTask.s00005844.output.s00000979:
   transitionVariable_callToTask.s00005844.outputCriterion.s00000939
   - transitionVariable_callToTask.s00006288.inputCriterion.s00000937 >= 0
 Effect_nonnegative_callToTask.s00005844.output.s00000981:
   transitionVariable_callToTask.s00005844.outputCriterion.s00000939
   - transitionVariable_callToTask.s00006285.inputCriterion.s00000937 >= 0
 Effect_nonnegative_callToTask.s00005844.output.s00001119:
   transitionVariable_callToTask.s00005844.outputCriterion.s00000939
   - transitionVariable_callToTask.s00006286.inputCriterion.s00000937 >= 0
 Effect_nonnegative_callToTask.s00005844.output.s00001125:
   transitionVariable_callToTask.s00005844.outputCriterion.s00000939
   - transitionVariable_callToTask.s00006287.inputCriterion.s00000937 >= 0
 Effect_nonnegative_callToTask.s00005844.output.s00001126:
   transitionVariable_callToTask.s00005844.outputCriterion.s00000939
   - transitionVariable_callToTask.s00006288.inputCriterion.s00000937 >= 0
 Effect_nonnegative_callToTask.s00005844.output.s00001552:
   transitionVariable_callToTask.s00005844.outputCriterion.s00000939
   - transitionVariable_callToTask.s00006285.inputCriterion.s00000937 >= 0
 Effect_nonnegative_callToTask.s00005844.output.s00001987:
   transitionVariable_callToTask.s00005844.outputCriterion.s00000939
   - transitionVariable_callToTask.s00006286.inputCriterion.s00000937 >= 0
 Effect_nonnegative_callToTask.s00005844.output.s00001988:
   transitionVariable_callToTask.s00005844.outputCriterion.s00000939
   - transitionVariable_callToTask.s00006287.inputCriterion.s00000937 >= 0
 Effect_nonnegative_callToTask.s00005844.output.s00001989:
   transitionVariable_callToTask.s00005844.outputCriterion.s00000939
   - transitionVariable_callToTask.s00006284.inputCriterion.s00000937 >= 0
 Effect_nonnegative_callToTask.s00005848.inputCriterion.s00000937.used:
   transitionVariable_callToTask.s00005848.inputCriterion.s00000937
   - transitionVariable_callToTask.s00005848.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00005848.output.s00000938:
   transitionVariable_callToTask.s00005848.outputCriterion.s00000939
   - transitionVariable_callToTask.s00006283.inputCriterion.s00000937 >= 0
 Effect_nonnegative_callToTask.s00005848.output.s00000956:
   transitionVariable_callToTask.s00005848.outputCriterion.s00000939
   - transitionVariable_callToTask.s00006283.inputCriterion.s00000937 >= 0
 Effect_nonnegative_callToTask.s00005848.output.s00000959:
   transitionVariable_callToTask.s00005848.outputCriterion.s00000939
   - transitionVariable_callToTask.s00006283.inputCriterion.s00000937 >= 0
 Effect_nonnegative_callToTask.s00006283.inputCriterion.s00000937.used:
   transitionVariable_callToTask.s00006283.inputCriterion.s00000937
   - transitionVariable_callToTask.s00006283.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00006283.output.s00000938:
   - transitionVariable_callToTask.s00003493.inputCriterion.s00000937
   + transitionVariable_callToTask.s00006283.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00006283.output.s00000956:
   - transitionVariable_callToTask.s00003493.inputCriterion.s00000937
   + transitionVariable_callToTask.s00006283.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00006283.output.s00000979:
   - transitionVariable_callToTask.s00003493.inputCriterion.s00000937
   + transitionVariable_callToTask.s00006283.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00006284.inputCriterion.s00000937.used:
   transitionVariable_callToTask.s00006284.inputCriterion.s00000937
   - transitionVariable_callToTask.s00006284.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00006284.output.s00000938:
   - transitionVariable_callToTask.s00005848.inputCriterion.s00000937
   + transitionVariable_callToTask.s00006284.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00006284.output.s00000956:
   - transitionVariable_callToTask.s00005848.inputCriterion.s00000937
   + transitionVariable_callToTask.s00006284.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00006284.output.s00000979:
   - transitionVariable_callToTask.s00005848.inputCriterion.s00000937
   + transitionVariable_callToTask.s00006284.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00006285.inputCriterion.s00000937.used:
   transitionVariable_callToTask.s00006285.inputCriterion.s00000937
   - transitionVariable_callToTask.s00006285.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00006285.output.s00000938:
   - transitionVariable_callToTask.s00005848.inputCriterion.s00000937
   + transitionVariable_callToTask.s00006285.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00006285.output.s00000956:
   - transitionVariable_callToTask.s00005848.inputCriterion.s00000937
   + transitionVariable_callToTask.s00006285.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00006285.output.s00000979:
   - transitionVariable_callToTask.s00005848.inputCriterion.s00000937
   + transitionVariable_callToTask.s00006285.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00006286.inputCriterion.s00000937.used:
   transitionVariable_callToTask.s00006286.inputCriterion.s00000937
   - transitionVariable_callToTask.s00006286.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00006286.output.s00000938:
   - transitionVariable_callToTask.s00005848.inputCriterion.s00000937
   + transitionVariable_callToTask.s00006286.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00006286.output.s00000956:
   - transitionVariable_callToTask.s00005848.inputCriterion.s00000937
   + transitionVariable_callToTask.s00006286.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00006286.output.s00000979:
   - transitionVariable_callToTask.s00005848.inputCriterion.s00000937
   + transitionVariable_callToTask.s00006286.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00006287.inputCriterion.s00000937.used:
   transitionVariable_callToTask.s00006287.inputCriterion.s00000937
   - transitionVariable_callToTask.s00006287.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00006287.output.s00000938:
   - transitionVariable_callToTask.s00005848.inputCriterion.s00000937
   + transitionVariable_callToTask.s00006287.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00006287.output.s00000956:
   - transitionVariable_callToTask.s00005848.inputCriterion.s00000937
   + transitionVariable_callToTask.s00006287.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00006287.output.s00000979:
   - transitionVariable_callToTask.s00005848.inputCriterion.s00000937
   + transitionVariable_callToTask.s00006287.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00006288.inputCriterion.s00000937.used:
   transitionVariable_callToTask.s00006288.inputCriterion.s00000937
   - transitionVariable_callToTask.s00006288.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00006288.output.s00000938:
   - transitionVariable_callToTask.s00005848.inputCriterion.s00000937
   + transitionVariable_callToTask.s00006288.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00006288.output.s00000956:
   - transitionVariable_callToTask.s00005848.inputCriterion.s00000937
   + transitionVariable_callToTask.s00006288.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_callToTask.s00006288.output.s00000979:
   - transitionVariable_callToTask.s00005848.inputCriterion.s00000937
   + transitionVariable_callToTask.s00006288.outputCriterion.s00000939
   >= 0
 Effect_nonnegative_process.s00000357##s00006282.input.s00000935:
   - transitionVariable_callToTask.s00001435.inputCriterion.s00000937
   + transitionVariable_process.s00000357##s00006282.inputCriterion.s00000937
   >= 0
 Effect_nonnegative_process.s00000357##s00006282.input.s00000950:
   - transitionVariable_callToTask.s00001435.inputCriterion.s00000937
   + transitionVariable_process.s00000357##s00006282.inputCriterion.s00000937
   >= 0
 Effect_nonnegative_process.s00000357##s00006282.outputCriterion.s00000939_omega:
   transitionVariable_process.s00000357##s00006282.outputCriterion.s00000939
   >= 0
Bounds
End
