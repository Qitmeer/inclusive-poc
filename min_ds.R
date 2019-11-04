source('functions.R')

# Suppose that there's a mainchain block Btx which confirms a transaction
# tx. The attacker wants to reverse Btx to double-spend the money
# transfered in tx. We also assume that there's no delay in honest network.
# That is, the honest blocks always form a chain.

N = 3  # the number of successors of Btx
q = 0.02  # the fraction of computational power the attacker holds

# This is the number of blocks the attacker has created in expectation during
# the time interval the honest network has created N blocks.
#
# Suppose that Btx is created at time t0, the current time is t, and the block
# creation rate is lambda, then the number of attacker blocks created in
# expectation during time interval t - t0 is
#
#   q * lambda * (t - t0).
#
# The Inclusive paper in contrast fixes N and averages over t. That is, let
#
#   t - t0 = N / (lambda * (1 - q)),
#
# so that:
attack_block_num_expect = q * N / (1 - q)

val = 1  # The reward from a block is normalized

# This is the minimal attack cost of secret chains based on
# formula (3) in the Inclusive paper. The i-th element is the cost
# of the secret chain of length h = i - 1.
min_cost_len_k = c(0)
for (i in 2:(N + 1)) {
  h = i - 1
  
  # formula (3) in the Inclusive paper
  min_cost_of_hth_block = (1 - gamma(N + 2 - h)) * val
  
  min_cost_len_k = c(min_cost_len_k, min_cost_len_k[i - 1] + min_cost_of_hth_block)
}

k = 0:N

# The i-th element is the probability that the attacker has created i - 1
# blocks during the interval t - t0.
k_blocks_probabilities = dpois(k, attack_block_num_expect)

# The i-th element is the probability that the attack will finally succeed
# under the condition that the attacker has created i - 1 blocks.
attack_succeed_probabilities_len_k = (q / (1 - q)) ** (N + 1 - k)

# The i-th element is the probability that the attack will finally fail
# under the condition that the attacker has created i - 1 blocks.
attack_fail_probabilities_len_k = 1 - attack_succeed_probabilities_len_k

# This is the probability that the length of secret chain is no greater than N
# and the attack succeed.
attack_succeed_probability_N = sum(k_blocks_probabilities * attack_succeed_probabilities_len_k)

# This is the probability that the length of secret chain is greater than N
# and the attack succeed. Actually if the secret chain is longer than N, i.e.
# the number of honest blocks, the attack definitely succeed.
N_plus_blocks_probability = 1 - ppois(N, attack_block_num_expect)

# This is the probability that the attack succeed.
attack_succeed_probability = attack_succeed_probability_N + N_plus_blocks_probability

# This is the expected attack cost without substracting the double spend money.
attack_cost_expect_no_DS = sum(k_blocks_probabilities * attack_fail_probabilities_len_k * min_cost_len_k)

# If we want the expected final cost of attack to be zero, we should let
#
#   attack_cost_expect_no_DS = attack_succeed_probability * DS
#
# so that:
DS = round(attack_cost_expect_no_DS / attack_succeed_probability)

print(DS)
