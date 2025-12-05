#include <libheif/heif.h>

typedef void (*mywriter) (const void *, size_t);

static struct heif_error
trampoline (struct heif_context *ctx,
            const void *data,
            size_t size,
            void *userdata) {
    struct heif_error result;
    mywriter writer = userdata;
    writer (data, size);
    result.code    = 0;
    result.subcode = 0;
    result.message = NULL;
    return result;
}

struct heif_error
heif_wrapper_context_write (struct heif_context *ctx,
                            mywriter callback) {
    struct heif_writer writer;
    writer.writer_api_version = 1;
    writer.write = trampoline;

    return heif_context_write (ctx, &writer, callback);
}
