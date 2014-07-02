#Applying the simulation on p. 242 to R. 

i = seq(1:10)
j = seq(1:10)

beta_11 = c(0,3,0)
# print(beta_11)
beta_12 = c(-1,0,0)
# print(beta_12)
beta_21 = c(-1,0,0)
# print(beta_21)
beta_22 = c(0,2,0)
# print(beta_22)

BETA_tot = matrix(c(beta_11,beta_12,beta_21,beta_22),ncol = 4)

print("BETA_tot")
print(BETA_tot)

# s = rep(1, 3)

# for ( k in i) {
	# for ( l in j) {
		 # if (
	# }
# }

s_0 = c(1,0,0)
s_1 = c(1,0,1)
s_2 = c(1,1,0)
s_3 = c(1,1,1)

S_tot = matrix(c(s_0,s_1,s_2,s_3), ncol = 4)
print("S_tot")
print(S_tot)

lambda_matrix = matrix(0,4,4)

# print(lambda_matrix)

print(exp(sum(BETA_tot[,1]*S_tot[,3])))

for (i in 1:4) {
	for (j in 1:4) {
		# Possible values of lambda given this model
		lambda_matrix[i,j] = exp(sum(BETA_tot[,i]*S_tot[,j])) 
	}
}

print(lambda_matrix)
