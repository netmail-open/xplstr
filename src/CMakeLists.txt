add_library(xplstr SHARED
  str.c
)


set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${ALL_CFLAGS} ${PTHREAD_CFLAGS} ${OPENSSL_CFLAGS}")

find_library(MEMMGR_LIBRARY
  NAMES libmemmgr memmgr
)

target_link_libraries(xplstr
	memmgr${BIT}
)

install(TARGETS xplstr DESTINATION /usr/lib)
