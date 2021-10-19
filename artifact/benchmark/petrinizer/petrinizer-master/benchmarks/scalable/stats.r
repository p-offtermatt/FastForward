#!/usr/bin/Rscript

library("data.table")
library("Hmisc")

folders, <-, c("Lamport", "Dijkstra", "Peterson", "Szymanski", "LeaderElectionDKR82", "LeaderElectionCR79", "Snapshot")

for (folder in folders) {
    infile <- paste(folder, "benchmark.out", sep="/")
    outfile <- paste(folder, "benchmark-agg.out", sep="/")
    timePlotFile <- paste(folder, "benchmark-time.pdf", sep="/")
    memoryPlotFile <- paste(folder, "benchmark-memory.pdf", sep="/")

    dataframe <- read.table(infile, header=TRUE)

    datatable <- data.table(dataframe)

    data <- datatable[,list(time=mean(user+system),memory=mean(memory/1024)),by=n]
    #data <- datatable[,list(time=mean(user+system),err_time=sd(user+system),memory=mean(memory/1024),err_memory=sd(memory)),by=n]

    write.table(data, file=outfile, row.names=FALSE)

    pdf(timePlotFile)
    plot(data$n,data$time,type="p",xlab="n",ylab="time (s)",main="Benchmark time consumption")
#with(data=data, expr=errbar(n, time, time+err_time, time-err_time, add=T, pch=1, cap=.1))
    dev.off()

    pdf(memoryPlotFile)
    plot(data$n,data$memory,type="p",xlab="n",ylab="memory (KiB)",main="Benchmark memory consumption")
#with(data=data, expr=errbar(n, memory, memory+err_memory, memory-err_memory, add=T, pch=1, cap=.1))
    dev.off()
}
