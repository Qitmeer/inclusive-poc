# This is the discount reward function defined by Example 1 in the paper
gamma <- function(c) {
  if (c <= 3) return (1)
  if (c < 10) return ((10 - c) / 7)
  return (0)
}

# This is the minimal cost of a failed attack performed by a secret chain of length k.
# The formula is defined by (3) in the paper.
attack_cost_len_k <- function(m, k, n, val) {
  discount = 0
  for (h in (m+1):(m+k)) {
    # Actually according to the paper, it should be
    #
    #   discount = discount + gamma(n + 2 - h).
    #
    # But I think it's wrong. It is written in the paper that
    #
    # 'If A_h is its h-th block (1 ≤ h ≤ k) then
    #
    #   pre(A_h).height < B_{tx}.height - 1 + m + h,
    #
    # or otherwise A_h necessarily references a block in H_{main} as its main parent'.
    #
    # But I think all the A_h should have
    #
    #   pre(A_h).height < B_{tx}.height,
    #
    # because A(t) contains no mainchain blocks, and if all the A_h form a secret chain
    # and B_{tx} is a mainchain block, then pre(A_h) should be the same for all
    # 1 <= h <= k and pre(A_h) should be a mainchain ancestor of B_{tx}.
    discount = discount + gamma(n + 1)
  }
  return ((k - discount) * val)
}

# This is the expected minimal attack cost defined by (4) in the paper.
attack_cost <- function(lambda, q, m, N, n, val) {
  return (0)
}