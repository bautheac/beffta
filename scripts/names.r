library(magrittr)


# Russel 3000 ####
names <- readxl::read_excel("data-raw/Russel 3000.xlsx", sheet = "RAY Index", skip = 2L) %>%
  dplyr::select(Ticker, PX_TO_BOOK_RATIO, PE_RATIO, EQY_DVD_YLD_EST) %>%
  dplyr::mutate_at(dplyr::vars(PX_TO_BOOK_RATIO, PE_RATIO, EQY_DVD_YLD_EST), dplyr::funs(as.numeric)) %>%
  dplyr::filter(complete.cases(.))

names[, c("PX_TO_BOOK_RATIO", "PE_RATIO", "EQY_DVD_YLD_EST")] <- scale(names[, c("PX_TO_BOOK_RATIO", "PE_RATIO", "EQY_DVD_YLD_EST")], 
                                                                       center = TRUE, scale = TRUE)

names %<>% dplyr::mutate(score = PX_TO_BOOK_RATIO * PE_RATIO * (1L / EQY_DVD_YLD_EST),
                         type = ifelse(score < median(score), "value", "growth")) %>%
  dplyr::arrange(score) %>%
  dplyr::select(ticker = Ticker, score, type)
  
# names <- rbind(dplyr::slice(names, 1L:(dplyr::n() / 20L)),
#                dplyr::slice(names, (dplyr::n() - (dplyr::n() / 20L)):dplyr::n()))
names <- rbind(dplyr::slice(names, 1L:25L),
               dplyr::slice(names, (dplyr::n() - 24L):dplyr::n()))


# xlsx::write.xlsx2(names, file = "data-raw/Russel 3000.xlsx", sheetName = "categories",
#             col.names = TRUE, row.names = FALSE, append = TRUE)

write.csv(names, file = "data-raw/names.csv", row.names = FALSE)







# S&P 500 ####
names <- readxl::read_excel("data-raw/S&P 500.xlsx", sheet = "SPX Index", skip = 2L) %>%
  dplyr::select(ticker = Ticker, price = PX_LAST, `price to book` = PX_TO_BOOK_RATIO, `price to earnings` = PE_RATIO, dividend = DVD_SH_LAST) %>%
  dplyr::mutate_at(dplyr::vars(price, `price to book`, `price to earnings`, dividend), dplyr::funs(as.numeric)) %>%
  dplyr::mutate(`inverse dividend yield` = price / dividend) %>%
  dplyr::select(ticker, `price to book`, `price to earnings`, `inverse dividend yield`) %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::mutate_at(dplyr::vars(`price to book`, `price to earnings`, `inverse dividend yield`), dplyr::funs(scale)) %>%
  dplyr::mutate(score = `price to book` / 3 + `price to earnings` / 3 + `inverse dividend yield` / 3) %>%
  dplyr::select(ticker, score)

names$score <- names$score[, 1L]

names %<>% dplyr::arrange(score)

value <- dplyr::mutate(head(names, 25L), category = "value") 
growth <- dplyr::mutate(tail(names, 25L), category = "growth") 

names <- rbind(dplyr::select(value, ticker, category), dplyr::select(growth, ticker, category))

write.csv(names, file = "data-raw/names.csv", row.names = FALSE)





# Russel 3000 value vs growth ####

## value ####
tickers <- readxl::read_excel("data-raw/Russel 3000 - value.xlsx", sheet = "RAV Index", skip = 2L) %>%
  dplyr::select(ticker = Ticker, price = PX_LAST, `book value` = BOOK_VAL_PER_SH, earnings = IS_EPS, dividend = IS_DIV_PER_SHR) %>%
  dplyr::slice(2L:n()) %>% dplyr::filter(`book value` != 0L, earnings != 0L, dividend != 0L) %>%
  dplyr::mutate_at(dplyr::vars(price, `book value`, earnings, dividend), dplyr::funs(as.numeric)) %>%
  dplyr::mutate(`price to book` = price / `book value`, `price to earnings` = price / earnings, `inverse dividend yield` = price / dividend) %>%
  dplyr::select(ticker, `price to book`, `price to earnings`, `inverse dividend yield`) %>%
  dplyr::filter(complete.cases(.)) %>%  
  dplyr::filter_at(dplyr::vars(-ticker), dplyr::all_vars(abs(.) <= (mean(.) + (3L * sd(.))))) %>%
  dplyr::mutate_at(dplyr::vars(`price to book`, `price to earnings`, `inverse dividend yield`), dplyr::funs(scale)) %>%
  dplyr::mutate(score = (`price to book` / 3L) + (`price to earnings` / 3L) + (`inverse dividend yield` / 3L))

tickers$score <- tickers$score[, 1L]; tickers %<>% dplyr::arrange(score)

value <- dplyr::mutate(head(tickers, 25L), category = "value") 

## growth ####
tickers <- readxl::read_excel("data-raw/Russel 3000 - growth.xlsx", sheet = "RAG Index", skip = 2L) %>%
  dplyr::select(ticker = Ticker, price = PX_LAST, `book value` = BOOK_VAL_PER_SH, earnings = IS_EPS, dividend = IS_DIV_PER_SHR) %>%
  dplyr::slice(2L:n()) %>% dplyr::filter(`book value` != 0L, earnings != 0L, dividend != 0L) %>%
  dplyr::mutate_at(dplyr::vars(price, `book value`, earnings, dividend), dplyr::funs(as.numeric)) %>%
  dplyr::mutate(`price to book` = price / `book value`, `price to earnings` = price / earnings, `inverse dividend yield` = price / dividend) %>%
  dplyr::select(ticker, `price to book`, `price to earnings`, `inverse dividend yield`) %>%
  dplyr::filter(complete.cases(.)) %>%  
  dplyr::filter_at(dplyr::vars(-ticker), dplyr::all_vars(abs(.) <= (mean(.) + (3L * sd(.))))) %>%
  dplyr::mutate_at(dplyr::vars(`price to book`, `price to earnings`, `inverse dividend yield`), dplyr::funs(scale)) %>%
  dplyr::mutate(score = (`price to book` / 3L) + (`price to earnings` / 3L) + (`inverse dividend yield` / 3L))

tickers$score <- tickers$score[, 1L]; tickers %<>% dplyr::arrange(score)

growth <- dplyr::mutate(tail(tickers, 25L), category = "growth") 


## out ####

tickers <- rbind(dplyr::select(value, ticker, category), dplyr::select(growth, ticker, category))

write.csv(tickers, file = "data-raw/names.csv", row.names = FALSE)





# S&P 500 value vs growth ####

## value ####
tickers <- readxl::read_excel("data-raw/S&P 500 - value.xlsx", sheet = "SVX Index", skip = 2L) %>%
  dplyr::select(ticker = Ticker, price = PX_LAST, `book value` = BOOK_VAL_PER_SH, earnings = IS_EPS, dividend = IS_DIV_PER_SHR) %>%
  dplyr::slice(2L:n()) %>% dplyr::filter(`book value` != 0L, earnings != 0L, dividend != 0L) %>%
  dplyr::mutate_at(dplyr::vars(price, `book value`, earnings, dividend), dplyr::funs(as.numeric)) %>%
  dplyr::mutate(`price to book` = price / `book value`, `price to earnings` = price / earnings, `inverse dividend yield` = price / dividend) %>%
  dplyr::select(ticker, `price to book`, `price to earnings`, `inverse dividend yield`) %>%
  dplyr::filter(complete.cases(.)) %>%  
  dplyr::filter_at(dplyr::vars(-ticker), dplyr::all_vars(abs(.) <= (mean(.) + (3L * sd(.))))) %>%
  dplyr::mutate_at(dplyr::vars(`price to book`, `price to earnings`, `inverse dividend yield`), dplyr::funs(scale)) %>%
  dplyr::mutate(score = (`price to book` / 3L) + (`price to earnings` / 3L) + (`inverse dividend yield` / 3L))

tickers$score <- tickers$score[, 1L]; tickers %<>% dplyr::arrange(score)

value <- dplyr::mutate(head(tickers, 25L), category = "value") 

## growth ####
tickers <- readxl::read_excel("data-raw/S&P 500 - growth.xlsx", sheet = "SGX Index", skip = 2L) %>%
  dplyr::select(ticker = Ticker, price = PX_LAST, `book value` = BOOK_VAL_PER_SH, earnings = IS_EPS, dividend = IS_DIV_PER_SHR) %>%
  dplyr::slice(2L:n()) %>% dplyr::filter(`book value` != 0L, earnings != 0L, dividend != 0L) %>%
  dplyr::mutate_at(dplyr::vars(price, `book value`, earnings, dividend), dplyr::funs(as.numeric)) %>%
  dplyr::mutate(`price to book` = price / `book value`, `price to earnings` = price / earnings, `inverse dividend yield` = price / dividend) %>%
  dplyr::select(ticker, `price to book`, `price to earnings`, `inverse dividend yield`) %>%
  dplyr::filter(complete.cases(.)) %>%  
  dplyr::filter_at(dplyr::vars(-ticker), dplyr::all_vars(abs(.) <= (mean(.) + (3L * sd(.))))) %>%
  dplyr::mutate_at(dplyr::vars(`price to book`, `price to earnings`, `inverse dividend yield`), dplyr::funs(scale)) %>%
  dplyr::mutate(score = (`price to book` / 3L) + (`price to earnings` / 3L) + (`inverse dividend yield` / 3L))

tickers$score <- tickers$score[, 1L]; tickers %<>% dplyr::arrange(score)

growth <- dplyr::mutate(tail(tickers, 25L), category = "growth") 


## out ####

tickers <- rbind(dplyr::select(value, ticker, category), dplyr::select(growth, ticker, category))

write.csv(tickers, file = "data-raw/names.csv", row.names = FALSE)









# FTSE 350 ####

tickers <- readxl::read_excel("data-raw/FTSE 350.xlsx", sheet = "NMX Index", skip = 2L) %>%
  dplyr::select(ticker = Ticker, price = PX_LAST, `book value` = BOOK_VAL_PER_SH, earnings = IS_EPS, dividend = IS_DIV_PER_SHR) %>%
  dplyr::slice(2L:n()) %>% dplyr::filter(`book value` != 0L, earnings != 0L, dividend != 0L) %>%
  dplyr::mutate_at(dplyr::vars(price, `book value`, earnings, dividend), dplyr::funs(as.numeric)) %>%
  dplyr::mutate(`price to book` = price / `book value`, `price to earnings` = price / earnings, `inverse dividend yield` = price / dividend) %>%
  dplyr::select(ticker, `price to book`, `price to earnings`, `inverse dividend yield`) %>%
  dplyr::filter(complete.cases(.)) %>%  
  dplyr::filter_at(dplyr::vars(-ticker), dplyr::all_vars(abs(.) <= (mean(.) + (3L * sd(.))))) %>%
  dplyr::mutate_at(dplyr::vars(`price to book`, `price to earnings`, `inverse dividend yield`), dplyr::funs(scale)) %>%
  dplyr::mutate(score = (`price to book` / 3L) + (`price to earnings` / 3L) + (`inverse dividend yield` / 3L))

tickers$score <- tickers$score[, 1L]; tickers %<>% dplyr::arrange(score)

value <- dplyr::mutate(head(tickers, 25L), category = "value") 
growth <- dplyr::mutate(tail(tickers, 25L), category = "growth") 

tickers <- rbind(dplyr::select(value, ticker, category), dplyr::select(growth, ticker, category))
write.csv(tickers, file = "data-raw/names.csv", row.names = FALSE)















