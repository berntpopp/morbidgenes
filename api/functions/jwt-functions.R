#### This file holds functions to encapsulate JWT token creation and decoding logic

#' Generate JWT (JSON Web Token)
#'
#' @description
#' Generates a JWT with a user's information and an expiration period. The JWT is
#' encoded using the HMAC algorithm.
#'
#' @param user A named list or data frame row containing user information (must include `user_id`,
#' `user_name`, `email`, `user_role`, `user_created`, and `orcid`).
#' @param secret A string representing the secret key used for encoding the JWT.
#' @param duration An integer representing the duration in seconds until the JWT expires.
#'
#' @return
#' A string representing the encoded JWT.
#'
#' @examples
#' user <- list(
#'   user_id = 123,
#'   user_name = "johndoe",
#'   email = "john.doe@example.com",
#'   user_role = "admin",
#'   user_created = "2020-01-01",
#'   orcid = "0000-0002-1825-0097"
#' )
#' secret <- "my_secret_key"
#' duration <- 3600 # 1 hour
#' jwt <- generate_jwt(user, secret, duration)
#' print(jwt)
#'
#' @export
generate_jwt <- function(user, secret, duration) {
  # Create the claim
  claim <- jwt_claim(
    user_id = user$user_id,
    user_name = user$user_name,
    email = user$email,
    user_role = user$user_role,
    user_created = user$created_at,
    orcid = user$orcid,
    iat = as.numeric(Sys.time()),
    exp = as.numeric(Sys.time()) + duration
  )

  # Encode the claim to generate JWT
  jwt_encode_hmac(claim, secret = secret)
}


#' Decode JWT (JSON Web Token)
#'
#' @description
#' Decodes a JWT and checks for its validity including the expiration. It raises an error
#' if the JWT is invalid or expired.
#'
#' @param jwt A string containing the JWT to be decoded.
#' @param secret A string representing the secret key used for decoding the JWT.
#'
#' @return
#' A list containing the decoded user information from the JWT if valid. If the token is expired,
#' it adds a `token_expired` boolean field to the list.
#'
#' @examples
#' jwt <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyX2lkIjoxMjMsInVzZXJfbmFtZSI6ImpvaG5kb2UiLCJlbWFpbCI6ImpvaG4uZG9lQGV4YW1wbGUuY29tIiwidXNlcl9yb2xlIjoiYWRtaW4iLCJ1c2VyX2NyZWF0ZWQiOiIyMDIwLTAxLTAxIiwib3JjaWQiOiIwMDAwLTAwMDItMTgyNS0wMDk3IiwiaWF0IjoxNjMwMzYwMDAwLCJleHAiOjE2MzA0NDY0MDB9.FO5RJf2oHq1T4Y1cQ_dRynjHvSUzPbVcX5IeZ2PLHVw"
#' secret <- "my_secret_key"
#' decoded_jwt <- decode_jwt(jwt, secret)
#' print(decoded_jwt)
#'
#' @export
decode_jwt <- function(jwt, secret) {
  # Decode JWT
  user <- jwt_decode_hmac(jwt, secret = secret)
  # Check expiration
  user$token_expired <- (user$exp < as.numeric(Sys.time()))

  if (is.null(jwt) || user$token_expired) {
    stop("Invalid or expired JWT", call. = FALSE)
  }

  user
}