# that's a bit ugly, but it makes all the rest much easier,
# and after all it's just a dev VM...
directory '/root' do
  mode 0755
end

# now we can import the test data
execute "psql -a -f #{::File.join(node['epgsql-test-setup']['test-data-dir'], 'test_schema.sql')}" do
  user 'postgres'
end
