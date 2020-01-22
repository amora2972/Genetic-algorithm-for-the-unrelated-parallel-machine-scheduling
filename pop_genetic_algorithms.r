#create a chromosome consist of:
#12 random float numbers between 0 and 1
#and second section
#and fitness value
createChromosome<- function(){
    chromosome <- c(runif(12),as.integer(runif(1,1,5)),as.integer(runif(1,1,4)),1,0)
    return(chromosome)
}

#this functions let us read csv tables
readFromTable <- function(tableName, rowName, columnName) {
  table <- read.csv(file = tableName, stringsAsFactors = FALSE)
  return(table[[rowName, columnName]])
}

#this function partitions the chromosome by selecting n minumum elements from job1 and job2 and 1 element of job3
partitioningChromosome<- function(chromosome){
  
  #getting job 1 genes which are 6 and divide them into 2 pieces
    jobOne <- chromosome[1:6]
    fstHalfJobOne <- chromosome[1:3]
    scndHalfJobOne <- chromosome[4:6]
    
    #getting job 2 genes which are 4 and divide them into 2 pieces
    jobTwo <- chromosome[7:10]
    fstHalfJobTwo <- chromosome[7:8]
    scndHalfJobTwo <- chromosome[9:10]
    
    #getting job 3 genes which are 2 and divide them into 2 pieces
    jobThree <- chromosome[11:12]
    fstHalfJobThree <- chromosome[11]
    scndHalfJobThree <- chromosome[12]
  
    #halfed chromosome vector
    partitionedChromosome <- c(
      fstHalfJobOne,
      scndHalfJobOne,
      fstHalfJobTwo,
      scndHalfJobTwo,
      fstHalfJobThree,
      scndHalfJobThree
    )
    #sequence of machines i have in my chromosome
    machines <- c(1,1,1,2,2,2,1,1,2,2,1,2)
    #sequence of jobs i have in my chromosome
    jobs <- c(1,1,1,1,1,1,2,2,2,2,3,3)
    
    #row binding halfed chromosomes with machines and jobs
    return(rbind(partitionedChromosome, machines, jobs))

}

#this function gets the minumum vlaues of every job regarding to the number assigned in second section
getMinumum<- function(chromosome, secondSection){
  
  #getting job one ordered in terms of indexes whihc is from index 1 to 6
  jobOne <- order(chromosome['partitionedChromosome',][1:6])
  jobTwo <- order(chromosome['partitionedChromosome',][7:10])
  #getting job TWO ordered in terms of indexes whihc is from index 7 to 10
  jobThree <- order(chromosome['partitionedChromosome',][11:12])
  #getting job three ordered in terms of indexes whihc is from index 11 to 12
  
  #getting the minumum of each job depending on its second section
  minumumChromosomes <- c(
    chromosome['partitionedChromosome',][jobOne[1:secondSection[1]]],
    chromosome['partitionedChromosome',][jobTwo[1:secondSection[2]]+6],
    chromosome['partitionedChromosome',][jobThree[1]+10]
  )
  
  #getting the machine in order to bind them with their corresponding gene
  machines <- c(
    chromosome['machines',][jobOne[1:secondSection[1]]],
    chromosome['machines',][jobTwo[1:secondSection[2]]+6],
    chromosome['machines',][jobThree[1]+10]
  )
  
  
  #getting the jobs in order to bind them with their corresponding gene
  jobs <- c(
    chromosome['jobs',][jobOne[1:secondSection[1]]],
    chromosome['jobs',][jobTwo[1:secondSection[2]]+6],
    chromosome['jobs',][jobThree[1]+10]
  )
  return(rbind(minumumChromosomes, machines, jobs))
}

calculateProcessingTime <- function(dataSet, secondSection){
  
  #print("my data set")
  #print(dataSet)
  
  #store the value of last job
  lastJobData <- 0
  processingTimeForMachine <- 0
  
  #get which machine is this set consist of...
  machine <- dataSet[2][1]
  
  if(machine == 1){
    #ordering genes from data set
    ordereDataSet <- order(dataSet['genes1',][TRUE])
    print("sequence of jobs in machine 1:")
    #from 1 until number of columns in data set
    for(i in 1:ncol(dataSet)){
      
      #get the job value
      job <- dataSet['jobsOne',][ordereDataSet[i]]
      
      print(job)
      #getting the job name by concatenating the number of job with the word Job
      jobName <- paste('Job',sep='',job)
      #print(jobName)
      
      #getting cost of data from csv table
      costOfData <- as.integer(readFromTable("cost.csv",machine,jobName))
      
      
      if(job == 1){
        #divide cost by second section for jobs 1 value
        costOfData <- costOfData / secondSection[1]
      }else if(job == 2){
        #divide cost by second section for jobs 2 value
        costOfData <- costOfData / secondSection[2]
      }
      #print("cost of data")
      #print(costOfData)
      
      #if this is the first cost then just sum it with the old vlaue which was 0
      if(lastJobData==0 || lastJobData == job){
        #sum the old value with the current cost
        processingTimeForMachine <- processingTimeForMachine + costOfData
      }else{
        #sum the delay with old cost and current cost
        timeValue <- readFromTable("m1_time.csv", lastJobData, jobName)
        processingTimeForMachine <- processingTimeForMachine + costOfData + timeValue
      }
      #assing the current job in order to be used as previouse job later on
      lastJobData = job
    }
  }else if(machine == 2){
    #ordering the genes
    ordereDataSet <- order(dataSet['genes2',][TRUE])
    print("sequence of job in machine 2:")
    #work from 1 until last column
    for(i in 1:ncol(dataSet)){
      #getting the job of the current index
      job <- dataSet['jobsTwo',][ordereDataSet[i]]
      
      print(job)
      #getting its name
      jobName <- paste('Job',sep='',job)
      #getting its cost
      costOfData <- as.integer(readFromTable("cost.csv",machine,jobName))
      
      
      if(job == 1){
        #dividing depending on secondSection of job1
        costOfData <- costOfData / secondSection[1]
      }else if(job == 2){
        #dividing depending on secondSection of job2
        costOfData <- costOfData / secondSection[2]
      }
      
      #if this is the first job or it matches with the latest job then there is no delay
      if(lastJobData==0 || lastJobData == job){
        processingTimeForMachine <- processingTimeForMachine + costOfData
      }else{
        #summing delay, cost and previous cost
        timeValue <- readFromTable("m2_time.csv", lastJobData, jobName)
        processingTimeForMachine <- processingTimeForMachine + costOfData + timeValue
      }
      #assiging the current job to lastJob as it will be used as previouse later on
      lastJobData = job
      
    }
  }
  #return the processig time 
  return(processingTimeForMachine)
}

crossOver<-function(chromosome1, chromosome2){
  #splitging chromo1 into halves with its second section
  fstHalfChromo1 <- c(chromosome1[1:6])
  sndHalfChromo1 <- c(chromosome1[7:12])
  fstHlafChromo1SecondSection <- c(chromosome1[13])
  sndHlafChromo1SecondSection <- c(chromosome1[14:15])
  
  
  #splitging chromo2 into halves with its second section
  fstHalfChromo2 <- c(chromosome2[1:6])
  sndHalfChromo2 <- c(chromosome2[7:12])
  fstHlafChromo2SecondSection <- c(chromosome2[13])
  sndHlafChromo2SecondSection <- c(chromosome2[14:15])
  
  
  mergedChromosomes1 <- c(fstHalfChromo1, sndHalfChromo2,fstHlafChromo1SecondSection,sndHlafChromo2SecondSection)
  mergedChromosomes2 <- c(fstHalfChromo2, sndHalfChromo1,fstHlafChromo2SecondSection,sndHlafChromo1SecondSection)
  
  mutation(mergedChromosomes1,mergedChromosomes2)
}
chromosomes <- c()

mutation <- function(chromosome1, chromosome2){
  
  #generating a random value to divide by and do mutation
  randomValue <- runif(1)
  
  #mutate 2 genes in chromosome 1
  chromosome1[2] <- chromosome1[2] / randomValue
  chromosome1[4] <- chromosome1[4] / randomValue
  
  #mutate 2 genes in chromosome 2
  chromosome2[2] <- chromosome2[2] / randomValue
  chromosome2[4] <- chromosome2[4] / randomValue

  #getting the fitness values for the new created chromosomes
  firstNewFitnessValue  <- structuringChromosome(chromosome1)
  secondNewFitnessValue <- structuringChromosome(chromosome2)
  
  #subtitue the newly created genes with old genes
  doSubtitution(firstNewFitnessValue, secondNewFitnessValue)
  
  #print the new chromosome
  print("changed chromosomes")
  print(chromosomes)
}

doSubtitution<-function(chromosome1, chromosome2){
  
  #order fitness values of chromosome
  orderedChromosomes <- c(order(chromosomes[,16]))
  
  #swiping....
  
  #if(chromosome1[16] < chromosomes[orderedChromosomes[10],][16]){
    chromosomes[orderedChromosomes[10],] <<- chromosome1
  #}
  #if(chromosome2[16] < chromosomes[orderedChromosomes[9],][16]){
    chromosomes[orderedChromosomes[9],] <<- chromosome2
  #}
}

#this function prepare the chromosome to get its fitnessValue calculated
structuringChromosome <- function(chromosome){
  
  #getting second section out of chromosome
  secondSection <- c(chromosome[13:15])
  
  #initalize the chromosome and binds it with its machines and jobs
  partionedChromosome<-partitioningChromosome(chromosome)
  
  #print(partionedChromosome)
  
  #getting minumum jobs from each one depending on its second section
  minumum <- getMinumum(partionedChromosome, secondSection)
  #print(minumum)
  
  #splitting machine1 and machine2 apart
  #where we put machines1' genes together and machines2' genes together
  jobsOne <- c()
  jobsTwo <- c()
  machine1 <- c()
  machine2 <- c()
  genes1 <- c()
  genes2 <- c()
  
  for(i in 1:ncol(minumum)){
    #if machine is 1
    if(minumum['machines',][i] == 1){
      
      #assign all the genes values to its corresponding variables
      machine1 <- c(machine1, minumum['machines',][i])
      jobsOne <- c(jobsOne,minumum['jobs',][i])
      genes1 <- c(genes1,minumum['minumumChromosomes',][i])
    }
    else{
      machine2 <- c(machine2, minumum['machines',][i])
      jobsTwo <- c(jobsTwo,minumum['jobs',][i])
      genes2 <- c(genes2,minumum['minumumChromosomes',][i])
    }
  }
  machineOneTime <- 0
  machineTwoTime <- 0
  #if machine one will be used then get its processing time
  if(!is.null(machine1[1])){
    machineOneTime <- calculateProcessingTime(rbind(genes1,machine1,jobsOne), secondSection)
  }
  print("mahine one time: ")
  print(machineOneTime)
  
  #if machine two will be used then get its processing time
  if(!is.null(machine2[1])){
    machineTwoTime <- calculateProcessingTime(rbind(genes2,machine2,jobsTwo),secondSection)
  }
  
  #get the maximum machine time
  max <- max(machineOneTime,machineTwoTime)
  print("machine Two Time: ")
  print(machineTwoTime)
  print("time taken: ")
  if(max == machineOneTime){
    print("machine one should be used")
  }else{
    print("machine two should be used")
  }
  print(max)
  #assign max as fitness value
  chromosome[16] <- max
  #return the chromosome with its fitness value added
  return(chromosome)
}
# chromosome, partitioning, minumum, fitnessValue, chromosomes
# 
#
main<- function(){
  
  #initialize 10 chromosomes
    for(i in 1:10){
        chromosome<- createChromosome()
        chromosomes <<- rbind(chromosomes, structuringChromosome(chromosome))
    }
    
  #print initialized chromosomed
    print("original Chromosomes")
    print(chromosomes)
    
    
    
    #print(fitnessValue)
    
    #do 50 iterations of crossover and mutation with regard to new fitness values
    for(i in 1:50){
      fitnessValue<-c()
      for(j in 1:nrow(chromosomes)){
        fitnessValue <- c(fitnessValue, chromosomes[j,][16])
      }  
      orderedFitnessValue <- order(fitnessValue)
      crossOver(chromosomes[orderedFitnessValue[1],][TRUE],chromosomes[orderedFitnessValue[2],][TRUE] )
    }
  }
main()