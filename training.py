################################################################################
#               HACKATHON PARTICIPANTS -- DO NOT EDIT THIS FILE                #
################################################################################

import sys
import time
import pickle

response = str(sys.argv[1])

exec(open("prepare_" + response + "_data.py").read())
exec(open(response + "_model.py").read())

################################################################################
# build the training data set and record the time required to do so.

tic = time.time()

training_data = eval("prepare_" + response + "_data(training = True)")

toc = time.time()

evaluation_file = open("./output/evaluation.txt", "a")
out = "seconds elapsed to prepare " + response + " training data | " + str(toc - tic) + "\n"
evaluation_file.write(out)

################################################################################
# Train the model and record the time required to do so.

tic = time.time()

trained_model = eval(response + "_model(training_data)")

toc = time.time()

#saveRDS(trained_model, file = paste0("./output/trained_", response, "_model.rds"))
pickle.dump(trained_model, open("./output/trained_" + response + "_model.pickle", "wb"))

out = "seconds elapsed to train the " + response + " model | " + str(toc - tic) + "\n"
evaluation_file.write(out)
evaluation_file.close()

###############################################################################
#                                 End of File                                  #
################################################################################
