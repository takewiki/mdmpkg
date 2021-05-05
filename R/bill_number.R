
#' 基础资料获取上级获取
#'
#' @param Numbers 明细编码使用.分隔
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mdm_getParentNumber()
#' mdm_getParentNumber('rds.01.001')
#' mdm_getParentNumber(c('rds.01.001.00001','rds.01.01'))
mdm_getParentNumber <- function(Numbers) {
  bb <-stringr::str_split(Numbers,pattern = "\\.")
  res <- lapply(bb, function(row){
    ncount <- length(row)
    data <- row[1:ncount-1]
    res <- paste0(data,collapse = ".")
    return(res)
  })
  #针对结果进行去列表处理
  res <- unlist(res)

  return(res)

}
