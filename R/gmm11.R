#' @title Grey prediction model gmm11 using R
#' @description Grey prediction model gmm11 using R
#' @param x raw data
#' @return predicted values of raw data
#' @examples
#' \dontrun{
#' x<-runif(100)
#' z<-gmm11(x)
#' z
#' }
#' @export
gmm11 <- function(x) {
  x1 <- cumsum(x)
  x0 <- x
  (b <- matrix(1, ncol = 2, nrow = length(x1)-1))
  
  for (i in seq_along(x1)-1) {
    b[i, 1] <- -(x1[i] + x1[i+1])/2
  }
  b
  (y <- x0[-1])
  
  (b_t_b <- t(b) %*% b)
  (b_t_b_1 <- solve(b_t_b))
  (b_t_y <- t(b) %*% matrix(y))
  (alpha_j <- b_t_b_1 %*% b_t_y)
  #得出预测模型
  (a <- alpha_j[1])
  (nu <- alpha_j[2])
  
  (x_j_1 <- (x0[1] - nu / a) * exp(- a * c(0:(length(x0)-1))) + nu / a)
  cat("公式为:\n", "x(k+1) =", x0[1] - alpha_j[2] / alpha_j[1], 
      "* exp(", alpha_j[1] , "* k)", alpha_j[2]/alpha_j[1], "\n")
  #2 累减
  lj <- function(x) {
    out <- array(dim = length(x))
    x_temp <- c(0, x)
    for(i in seq_along(x)) {
      out[i] <- x_temp[i+1] - x_temp[i]
    }
    as.numeric(out)
  }
  (x_j_0 <- lj(x_j_1))
  
  #3 计算绝对误差序列和相对误差序列
  (theta <- round(abs(x_j_0 - x0), 6))#保留小数点后6位
  (big_theta <- round(theta / x_j_0, 8))
  
  #第六步 进行关联度检验
  (nitheta <- (min(theta) + 0.5 * max(theta)) / (theta + 0.5 *max(theta)))
  # 2 关联度
  (r <- mean(nitheta))
  
  # 第七步 后验差检验
  # 1原始序列标准差
  (s1 <- sd(x0))
  # 2残差标准差
  (s2 <- sd(theta))
  # 3 计算C
  (c <- s2 / s1)
  (ei <- abs(theta - mean(theta)))
  x_next <- (x0[1] - nu / a) * (exp(- a * (length(x0)+1)) - exp(- a * length(x0)))
  list(a=a, 
       mu=nu, 
       jdwc=theta,# 绝对误差
       glxs = nitheta, #关联系数
       r=r, #关联度
       c = round(c, 6), #
       ei = ei, #小误差概率
       x_next = x_next #预测值
  )
}