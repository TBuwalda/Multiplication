rm(list = setdiff(ls(), lsf.str()))

library(ggplot2)
library(dplyr)

source("probabilityOfRetrieval.r")  

shinyServer(function(input, output, session) {

  load("dat.Rdata")

  params <- list(activationNoise = 0.2,
                  decay = 0.5,
                  mismatchPenalty = 5,
                  threshold = -1,
                  optimizedLearning = TRUE,
                  similarityFunction = "linear",
                  inputAct = 0)

  chunks <- NULL
  for(addend1 in 1:9) {
    for(addend2 in 1:9) {
      tmp <- data.frame(name = paste(addend1, "x", addend2),
                references = 1,
                lifetime = 1,
                timeSinceLastRef = 1,
                slot1 = addend1,
                slot2 = "x",
                slot3 = addend2,
                slot4 = addend1 * addend2,
                slot5 = NA,
                slot6 = NA)
      if(is.null(chunks)) {
        chunks <- tmp
      } else {
        chunks <- rbind(chunks, tmp)
      }
    }
  }

  observe({
    input$decay
    input$mp
    input$ans
    input$ol
    input$simfun

    updateSelectInput(session, "param_group",
         selected = "Manual")
  })

  observe({
    if(input$param_group == "Christian") {
      params$similarityFunction <<- "christian"
      params$activationNoise <<- 0.25
      params$mismatchPenalty <<- 1.5
      params$threshold <<- -1
      params$optimizedLearning <<- TRUE
      params$inputAct <<- 0
    }
  })

  output$chunks <- renderDataTable({
    input$problem
    chunks
  })

  output$problemsSoFar <- renderDataTable({
    location <- input$problem
    if(location != 0) {
      load("fixedOrder.Rdata") # fixedOrder
      tmp <- fixedOrder[1:location,]
      tmp
    } else {
      data.frame(name="", addend1="No", operator="Problems", addend2="Have", order="Been", rt.model="Presented", rt.data="Yet", rating="")
    }
  })

  output$main_plot <- renderPlot({
    input$request_button
    input$param_group
    #resetRefs()

    slot1 <- isolate(input$request_slot1)
    slot2 <- isolate(input$request_slot2)
    slot3 <- isolate(input$request_slot3)

    resptime <- input$response_time
    location <- input$problem

    name <- c(slot1, slot2, slot3)
    locationInFixedList(location, resptime)

    params$decay <<- input$decay
    params$mismatchPenalty <<- input$mp
    #params$threshold <<- input$rt
    #params$inputAct <<- input$inputAct
    params$activationNoise <<- input$ans
    params$optimizedLearning <<- ifelse(input$ol == "on", TRUE, FALSE)
    params$similarityFunction <<- input$simfun

    probs <- chunkChoice(name, params, chunks)
    probs$addend1 <- as.numeric(substr(probs$chunk, 1, 1))
    probs$addend2 <- as.numeric(substr(probs$chunk, 5, 5))
    probs$answer <- probs$addend1 * probs$addend2

    toPlotDat <- dat[dat$addend1 == slot1 & dat$addend2 == slot3, c("answer", "ratio")]
    toPlotDat <- rename(toPlotDat, probability = ratio)
    toPlotDat$model <- 0
    if(nrow(toPlotDat) > 20) {
      tmp <- toPlotDat[order(toPlotDat$probability, decreasing=TRUE),]
      toPlotDat <- tmp[1:20,]
    }

    toPlotProb <- with(probs, aggregate(list(probability=probability), list(answer=answer), sum))
    toPlotProb$answer <- as.factor(toPlotProb$answer)
    toPlotProb <- toPlotProb[order(toPlotProb$probability, decreasing = TRUE),]
    toPlotProb <- toPlotProb[toPlotProb$probability > 0,]
    toPlotProb$model <- 1
    if(nrow(toPlotProb) > 20) {
      tmp <- toPlotProb[order(toPlotProb$probability, decreasing=TRUE),]
      toPlotProb <- tmp[1:20,]
    }

    missingProb <- droplevels(toPlotDat[toPlotDat$answer %out% toPlotProb$answer,]$answer)
    missingDat <- droplevels(toPlotProb[toPlotProb$answer %out% toPlotDat$answer,]$answer)
    toPlotProb <- rbind(toPlotProb, data.frame(answer=missingProb,probability=0, model=1))
    toPlotDat <- rbind(toPlotDat, data.frame(answer=missingDat,probability=0, model=0))

    toPlot <- rbind(toPlotProb, toPlotDat)
    #toPlot$probability <- ifelse(is.na(toPlot$probability), 0, toPlot$probability)
    #toPlot$ratio <- ifelse(is.na(toPlot$ratio), 0, toPlot$ratio)
    
    if(input$compare_data) {
      ordering <- toPlot[toPlot$model == 0,]$answer

      ggplot(data = toPlot, aes(x = answer, y = probability)) +
        geom_bar(stat = "identity", position = "dodge", aes(color=factor(model), fill = factor(model))) +
        scale_x_discrete(limits = ordering) +
        theme(text = element_text(size=16)) +
        coord_cartesian(ylim=c(0, 0.1)) + 
        scale_fill_manual(values=c("turquoise3", "gray33"), guide=FALSE) +
        scale_color_manual(values=c("gray33", "gray33"), guide=FALSE)
    } else {
      ordering <- toPlotProb$answer

      ggplot(data = toPlotProb, aes(x = answer, y = probability, fill="manual")) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_x_discrete(limits = ordering) +
        theme(text = element_text(size=16)) +
        coord_cartesian(ylim=c(0, 0.1)) +
        scale_fill_manual(values=c("gray33"), guide=FALSE) + 
        scale_color_manual(values=c("gray33"), guide=FALSE)
    }

    #hist(probs$probability,
    #  probability = TRUE,
    #  xlab = "Probability of Retrieval",
     # main = "Probabilities")

    #if (input$individual_obs) {
    #  rug(faithful$eruptions)
    #}

    #if (input$density) {
    #  dens <- density(faithful$eruptions,
    #      adjust = input$bw_adjust)
    #  lines(dens, col = "blue")
    #}

  })

  locationInFixedList <- function(location, resptime) {
    if(location != 0) {
      load("fixedOrder.Rdata") # fixedOrder
      tmp <- fixedOrder[1:location,]
      tmp$rt <- ifelse(resptime == "mtime", tmp$rt.model, tmp$rt.data)
      tmp$time <- cumsum(tmp$rt + 1)

      lifetime <- 1 + max(tmp$time)
      chunks$lifetime <<- lifetime

      if(sum(chunks$name %out% unique(tmp$name)) != 0) {
        chunks[chunks$name %out% unique(tmp$name),]$timeSinceLastRef <<- lifetime
      }

      for(name in unique(tmp$name)) {
        references <- 1 + nrow(tmp[tmp$name == name,])
        
        if(nrow(tmp[tmp$name == name,]) != 0) {
          timeSinceLastRef <- unique(lifetime) - max(tmp[tmp$name == name,]$time) 
        } else {
          timeSinceLastRef <- 1
        }
        if(nrow(chunks[chunks$name == name,]) != 0) {
          chunks[chunks$name == name,]$references <<- references
          chunks[chunks$name == name,]$timeSinceLastRef <<- timeSinceLastRef
        }
      }
    } else {
      chunks$timeSinceLastRef <- 1
      chunks$references <- 1
      chunks$lifetime <- 1
    }
  }

  updateReferences <- function(name, references) {
    chunks[chunks$name == name,]$references <<- references
  }

  updateLifetime <- function(name, lifetime) {
    chunks[chunks$name == name,]$lifetime <<- lifetime
  }

  updateTimeSinceLastRef <- function(name, timeSinceLastRef) {
    chunks[chunks$name == name,]$timeSinceLastRef <<- timeSinceLastRef
  }

  output$distPlot <- renderPlot({
      # Take a dependency on input$goButton
      input$goButton

      # Use isolate() to avoid dependency on input$obs
      dist <- isolate(rnorm(input$obs))
      hist(dist)
    })
})