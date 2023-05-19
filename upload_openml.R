library(mlr3oml)

publish_task = function(data_id, type, estimation_procedure, target = NULL, ...) {
  api_key = mlr3oml:::get_api_key(mlr3oml:::get_server(FALSE))
  estimation_procedure = checkmate::assert_integerish(estimation_procedure, len = 1L)

  task_type_id = switch(type,
    classif = "1",
    regr = "2"
  )

  doc = xml2::xml_new_document()
  task = xml2::xml_add_child(doc, "oml:task_inputs", "xmlns:oml" = "http://openml.org/openml")
  xml2::xml_add_child(task, "oml:task_type_id", task_type_id)
  xml2::xml_add_child(task, "oml:input", name = "source_data", data_id)
  if (!is.null(target)) {
    xml2::xml_add_child(task, "oml:input", name = "target_feature", target)
  }
  xml2::xml_add_child(task, "oml:input", name = "estimation_procedure",
    as.character(estimation_procedure)
  )

  withr::defer(unlink(desc_path))
  desc_path = tempfile(fileext = ".xml")
  xml2::write_xml(x = doc, file = desc_path)

  response = httr::POST(
    url = sprintf("%s/task", mlr3oml:::get_server(FALSE)),
    body = list(
      description = httr::upload_file(desc_path)
    ),
    query = list(api_key = api_key)
  )

  response_list = xml2::as_list(httr::content(response))
  if (httr::http_error(response)) {
    if (isTRUE(response_list$error$code[[1L]] == "614")) { # Task already exists.
      info = response_list$error$additional_information[[1L]]
      id = as.integer(substr(info, 17L, nchar(info) - 1L))
      mlr3misc::messagef("Task already exists with id %s.", id)
      return(id)
    } else {
      mlr3misc::warningf(
        paste(response_list$error$message, response_list$error$additional_information, collapse = "\n")
      )
      return(response)
    }
  } else {
    id = as.integer(response_list$upload_task$id[[1L]])
    return(id)
  }
}

col = ocl(328)

otasks = lapply(col$task_ids, function(i) otsk(i))

ids = list()

for (otask in otasks) {

  if (otask$data$nrow <= 1000) {
    # 10 x 10
    estimation_procedure =  9
  } else if (otask$data$nrow <= 100000) {
    # 10 x 1
    estimation_procedure = 7
  } else {
    # 33% holdout
    estimation_procedure = 12
  }

  id = publish_task(
    data_id = otask$data_id,
    type = "regr",
    estimation_procedure = estimation_procedure,
    target = otask$target_names,
    api_key = API_KEY
  )

  ids[[length(ids) + 1L]] = id
}







ids = readRDS("/home/sebi/r/regbms/finalids.rds")

publish_task(ids[1], "regr", 1)

