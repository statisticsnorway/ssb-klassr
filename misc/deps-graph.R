library(DependenciesGraphs)

# Prepare data
dep <- DependenciesGraphs::envirDependencies("package:klassR")

# visualization
plot(dep)

library(igraph)

klassr_deps <- graph_from_data_frame(
  d = dep$fromto,
  vertices = dep$Nomfun,
  directed = TRUE
)

plot(klassr_deps)

check_connect_callers <-
  induced_subgraph(
    klassr_deps,
    subcomponent(
      klassr_deps,
      v = V(klassr_deps)[label == "check_connect"],
      mode = "out"
    )
  )

check_connect_callers <- delete_vertices(
  check_connect_callers,
  V(check_connect_callers)[
    label %in%
      c(
        # exlude aliases
        "ApplyKlass",
        "SearchKlass",
        "GetName",
        "CorrespondList",
        "GetFamily",
        "ListKlass",
        "ListFamily",
        "GetKlass",
        "GetVersion"
      )
  ]
)

library(visNetwork)

visIgraph(check_connect_callers, idToLabel = FALSE) |>
  visNodes(
    label = V(check_connect_callers)$label
  )
