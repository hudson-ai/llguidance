//
// This interface needs to be implemented by the WASM binary
//

// Tokens are assumed to be at most 32 bit.
// Typical models range 30k (LLAMA) to 100k (GPT4) tokens.
typedef uint32_t token_t;

// Called first, after instantiating WASM module.
void gvm_init(void);

// Called once per module, to get a GVM for a specific query
Gvm *gvm_create(void);

// If a query is split into several (eg., during beam-search, or when returning several results)
// this is called to get GVM for the sub-query.
Gvm *gvm_clone(Gvm *parent);

// These two are called after gvm_create() and gvm_clone() on the fresh GVM.
// They should return the buffers that the WASM code has to allocated and keep around
// until relevant gvm_free().

// Return buffer where the prompt will be written. `size` is number of tokens in the prompt.
token_t *gvm_get_prompt_buffer(Gvm *gvm, uint32_t size);

// Return the buffer where the WASM code will write logit biases after
// gvm_process_prompt() and gvm_append_token().
// Size of number of biases (which equals size of the vocabulary).
float *gvm_get_logit_bias_buffer(Gvm *gvm, uint32_t size);

// This called once, when the GVM should process the prompt in its buffer.
// It should set the values in logit bias buffer.
void gvm_process_prompt(Gvm *gvm);
// The logical type (if WASM would allow such things) of this function is:
// float[vocab_size] gvm_process_prompt(Gvm *gvm, token_t[] prompt);

// This is called after a token is sampled.
// It should set the values in logit bias buffer.
void gvm_append_token(Gvm *gvm, token_t tok);
// The logical type (if WASM would allow such things) of this function is:
// float[vocab_size] gvm_append_token(Gvm *gvm, token_t tok);

// This is called for GVMs that no longer needed (eg. because generation completed,
// or beam-search branch was cut).
void gvm_free(Gvm *gvm);

//
// This interface is available to the WASM binary
//

// Log a string.
void gvm_host_print(const uint8_t *ptr, uint32_t size);

// Read binary representation of TokTrie.
// Always returns the size of the trie, will write up to `size` bytes to `dst`.
uint32_t gvm_host_read_token_trie(uint8_t *dst, uint32_t size);
