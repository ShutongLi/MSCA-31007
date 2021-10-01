datapath = './'
data=read.table(paste(datapath,'Week1_Test_Sample.csv',sep = '/'),header = TRUE)
joint_distribution = table(data) / dim(data)[1]
u_Marginal = apply(joint_distribution, MARGIN = 1, sum)
v_Marginal = apply(joint_distribution, MARGIN = 2, sum)
# P(u|v=4) = P(u,v) / P(v = 4)
p_v4 = v_Marginal[4]
u_Conditional_v = joint_distribution[,4] / p_v4
# P(v|u=3) = P(u,v) / P(u=3)
p_u3 = u_Marginal[3]
v_Conditional_u = joint_distribution[3,] / p_u3
res <-list(Joint_distribution=joint_distribution,
           u_Marginal = u_Marginal,
           v_Marginal = v_Marginal,
           u_Conditional_v = u_Conditional_v,
           v_Conditional_u = v_Conditional_u          )
saveRDS(res, file = paste(datapath,'result.rds',sep = '/'))

  